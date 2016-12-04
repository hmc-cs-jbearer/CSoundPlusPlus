package cspp

import scala.collection.immutable.HashSet
import scala.util.parsing.input.Positional

import absyn._
import AbsynSugar._
import CsppDagNodes._

object CsppTranslator {

  abstract class CsType(val typeId: Char) {
    override def toString = typeId.toString
  }

  object InitRate extends CsType('i')
  object AudioRate extends CsType('a')

  // Keeps track of how many things (local variables etc) have been created.
  // This allows us to always generate unique names.
  case class Context(locals: Int,                 // Number of i-rate variables in scope
                     instrs: Int,                 // Number of instrument definitions
                     localScope: HashSet[Ident],  // Named local variables (as opposed to anonymous
                                                  // variables of the form i1, i2, etc)
                     globalScope: HashSet[Ident]) // Named global variables

  // instr 0 is reserved in CSound, so we have to start out with 1 instrument
  object EmptyContext extends Context(0, 1, HashSet(), HashSet())


  type CsLine = String
  type CsLines = Seq[CsLine]

  var debugMode = false

  def apply(dag: Seq[StmtNode], debug: Boolean = false): Either[CsppCompileError, CsLines] = {
    CsppTranslator.debugMode = debug
    try {
      val (_, lines) = translateStmtNodes(EmptyContext, dag)
      Right(lines)
    } catch {
      case e: CsppTranslateError => Left(e)
    }
  }

  // Get the csound type (i for init-rate, a for audio-rate, etc) of a CSPP expression
  def csType[T <: TypeAnnotation with CsppPositional](elem: T) = elem.ty match {
    case Some(Number) | Some(Function(Number, _)) => InitRate
    case Some(_)                                  => AudioRate
    case None                                     =>
      throw new CsppTranslateError(elem.loc, s"Untyped expression ${elem}.")
  }

  def typeOf[T <: TypeAnnotation with CsppPositional](elem: T): CsppType = elem match {
    case CompNode(_, _, expr) => typeOf(expr)

    case _ => elem.ty match {
      case Some(Function(ty, _)) => ty
      case Some(ty) => ty
      case None =>
        throw new CsppTranslateError(elem.loc, s"Untyped expression ${elem}.")
    }
  }

  // Generate an instrument name that is unique to the given context
  def instrName(context: Context) = {
    val id = context.instrs
    (context.copy(instrs = id + 1), id.toString)
  }

  // User-defined opcode names are mangled to avoid clashes with CSound built-in opcodes
  def opcodeName(id: Ident) = id match {
    case Ident(name) => s"cspp_${name}"
  }

  def localName(id: Ident) = s"i${id.name}"

  def globalName(id: Ident) = s"gi${id.name}"

  // Generate a local variable name that is unique to the given context
  def localAnon(context: Context) = {
    val id = context.locals
    (context.copy(locals = id + 1), s"i$id")
  }

  def isLocal(context: Context, id: Ident) = context.localScope contains id
  def isGlobal(context: Context, id: Ident) = context.globalScope contains id

  def extendLocalScope(context: Context, newVar: Ident): Context
    = extendLocalScope(context, Seq(newVar))
  def extendLocalScope(context: Context, newVars: Seq[Ident]): Context
    = context.copy(localScope = context.localScope ++ newVars)
  def extendGlobalScope(context: Context, newVar: Ident): Context
    = extendGlobalScope(context, Seq(newVar))
  def extendGlobalScope(context: Context, newVars: Seq[Ident]): Context
    = context.copy(globalScope = context.globalScope ++ newVars)

  abstract class LogLevel(name: String) {
    override def toString = name
  }
  object DEBUG extends LogLevel("DEBUG")
  object INFO extends LogLevel("INFO")
  object WARNING extends LogLevel("WARNING")
  object ERROR extends LogLevel("ERROR")

  def debug(lines: CsLine*): CsLines = if (debugMode) lines else Seq()

  def _log(printf: String, level: LogLevel, name: String, fmt: String, params: Seq[String]): CsLines = {
    val fmtStr = "\"" ++ s"CSPPLOG $level name=$name $fmt" ++ "\\n\""
    debug(printf ++ " " ++ (fmtStr +: params).mkString(", "))
  }

  // Generate a CSound prints statement
  def log(level: LogLevel, name: String, fmt: String, params: String*): CsLines =
    _log("prints", level, name, fmt, params)

  // Generate a CSound printks statement
  def logk(level: LogLevel, name: String, fmt: String, params: String*): CsLines =
    _log("printks", level, name, fmt,
      // printks takes an i-rate parameter specifiying the time in seconds between print statements.
      // 0 means print at the control rate.
      "0" +:params)

  def loga(level: LogLevel, name: String, sig: String, fmt: String, params: String*): CsLines =
    debug(s"k$sig downsamp $sig") ++ _log("printks", level, name, fmt + " samp=%f",
      // printks takes an i-rate parameter specifiying the time in seconds between print statements.
      // 0 means print at the control rate.
      ("0" +: params) :+ s"k$sig")

  /**
   * Create a new user defined opcode which implements a CSound++ component. The opcode may have one
   * or more a-rate inputs, and will produce at least one a-rate output.
   */
  def component(name: String,
                inputs: Seq[String],
                params: Seq[String],
                body: CsLines,
                outVars: Seq[String]): CsLines = {

    require(outVars.length > 0)

    // Generate variables to hold the input signals, of the form a1, a2, etc.
    val allParams = inputs ++ params.map(localName(_))

    val inTys = "a" * inputs.length + "i" * params.length
    val outTys = "a" * outVars.length

    // The lines to declare the opcode and accept inputs via xin
    val init = if (allParams.isEmpty) {
      Seq(s"opcode $name, $outTys, 0")
    } else {
      Seq(s"opcode $name, $outTys, $inTys", s"${allParams.mkString(", ")} xin")
    }

    init ++
    body ++ Seq(
    s"xout ${outVars.mkString(", ")}",
    "endop",
    "") // End with a blank line just for readability
  }

  /**
   * Create a new user defined opcode which implemets a CSound++ scalar-valued function. All inputs
   * must be i-rate, and the opcode will output exactly one i-rate value.
   */
  def function(name: String, params: Seq[String], body: CsLines, outVar: String): CsLines = {
    // The lines to declare the opcode and accept inputs via xin
    val init = if (params.isEmpty) {
      Seq(s"opcode $name, i, 0")
    } else {
      Seq(s"opcode $name, i, ${"i" * params.length}",
          params.map(localName(_)).mkString(",") + " xin")
    }

    init ++
    body ++ Seq(
    s"xout $outVar",
    "endop",
    "") // End with a blank line just for readability
  }

  // Create a new instrument
  def instrDefine(name: String, body: CsLines, outVar: String): CsLines =
    s"instr ${name}" +:
    "iamp ampmidi 1" +:
    "ifreq cpsmidi" +:
    (log(INFO, "note_on", s"instr=$name amp=%f freq=%f", "iamp", "ifreq") ++
    body ++
    loga(DEBUG, "sample", outVar, s"instr=$name") ++ Seq(
    s"out $outVar",
    "endin",
    "")) // End with a blank line just for readability

  def translateStmtNodes(context: Context, nodes: Seq[StmtNode]): (Context, CsLines) =
    if (nodes.isEmpty) {
      (context, Seq())
    } else {
      val (context2, headLines) = translateStmtNode(context, nodes.head)
      val (context3, tailLines) = translateStmtNodes(context2, nodes.tail)
      (context3, headLines ++ tailLines)
    }

  def translateStmtNode(context: Context, node: StmtNode): (Context, CsLines) = node match {
    case AssignNode(inputs, outputs, Assignment(id, params, expr)) => {
      val localContext = extendLocalScope(context, params)

      val name = opcodeName(id)
      val paramNames = params.map(_.name)

      // We ignore the context we get out, because any locals defined in this body go out of scope
      // at the end of the opcode definition. Thus, we can reuse their names in the next statement.
      val (_, body, scalarOutputs) = translateExpr(localContext, expr)

      val opcodeLines = typeOf(expr) match {
        case _: Component => component(name, inputs, paramNames, body, outputs)
        case _            => function(name, paramNames, body, scalarOutputs.head)
      }

      (context, opcodeLines)
    }

    case InstrNode(inputs, outputs, Instrument(channels, expr)) => {
      require(inputs.length == 0)
      require(outputs.length == 1)
      val outName = outputs.head

      val (context2, instrId) = instrName(context)
      val localContext = extendLocalScope(context2, Seq(Ident("freq"), Ident("amp")))

      // We ignore the final context for the same reason as above
      val (_, body, _) = translateExpr(localContext, expr)

      val instrLines = instrDefine(instrId, body, outName)
      val (context3, channelLines) = translateChannels(context2, instrId, channels)
      (context3, instrLines ++ channelLines)
    }
  }

  // Generate CSound code to map channels indicated by channels to the given instrument
  def translateChannels(context: Context, instrId: String, channels: Seq[Expr]): (Context, CsLines) = {
    if (channels.isEmpty) {
      (context, Seq("")) // Empty line for readability
    } else {
      val (context2, channelLines, channelVars) = translateExpr(context, channels.head)
      require(channelVars.length == 1)
      val mapLines = channelLines ++ Seq(s"massign ${channelVars.head}, $instrId")
      val (context3, restLines) = translateChannels(context2, instrId, channels.tail)
      (context3, mapLines ++ restLines)
    }
  }

  /**
   * Generate code to store the value of expr in one or more local variables.
   * This function simply allocates any necessary local variables to store the output and delegates
   * to the overload below.
   */
  def translateExpr(context: Context, expr: Expr): (Context, CsLines, Seq[String]) = expr match {
    case Num(n, _) => {
      val (context2, outVar) = localAnon(context)
      (context2, Seq(s"$outVar = $n"), Seq(outVar))
    }

    case BinOp(l, op, r, _) => {
      val (context2, leftLines, leftVars) = translateExpr(context, l)
      val (context3, rightLines, rightVars) = translateExpr(context2, r)
      val (context4, outVar) = localAnon(context3)

      require(leftVars.length == 1)
      require(rightVars.length == 1)

      val opStr = op match {
        case Plus => "+"
        case Minus => "-"
        case Times => "*"
        case Divide => "/"
      }

      (context4,
       leftLines ++ rightLines :+ s"${outVar} = ${leftVars.head} $opStr ${rightVars.head}",
       Seq(outVar))
    }

    // Unwrapped applications are number-valued functions
    case app: Application => {
      val (context2, outVar) = localAnon(context)
      // No audio inputs to a number-valued function
      opcodeCall(context, app, Seq(), Seq(outVar))
    }

    case CompNode(inputs, outputs, expr) => expr match {
      // Applications wrapped in DAG nodes are components
      case app: Application => opcodeCall(context, app, inputs, outputs)

      // We can treat chains and parallel blocks the same now because the information by which they
      // differ is encoded in the DAG
      case Chain(body, _) => translateBlock(context, body, outputs)
      case Parallel(body, _) => translateBlock(context, body, outputs)
    }

    case c: Chain =>
      throw new CsppTranslateError(expr.loc, "Chain expression was not transformed to DAG node.")

    case p: Parallel =>
      throw new CsppTranslateError(expr.loc, "Parallel expression was not transformed to DAG node.")
  }

  def translateBlock(
    context: Context, body: Seq[Expr], outputs: Seq[String]): (Context, CsLines, Seq[String]) =
    if (body.isEmpty) {
      // An empty block is an effect which just leaves it's inputs unchanged
      (context, Seq(), outputs)
    } else {
      val (context2, lines) = translateComponents(context, body)
      (context2, lines, outputs)
    }

  def translateComponents(context: Context, comps: Seq[Expr]): (Context, CsLines) =
    if (comps.isEmpty) {
      (context, Seq())
    } else {
      val (context2, headLines, _) = translateExpr(context, comps.head)
      val (context3, tailLines) = translateComponents(context2, comps.tail)
      (context3, headLines ++ tailLines)
    }

  // Call an opcode with the given args, returning the name of the output variables
  def opcodeCall(context: Context, app: Application, inputs: Seq[String], outputs: Seq[String]):
    (Context, CsLines, Seq[String]) = {

    val Application(Ident(opName), args, Some(ty)) = app

    if (isLocal(context, opName)) {
      require(args.length == 0) // Typechecker should have ensured this
      require(ty == Number)

      val (context2, outName) = localAnon(context)
      (context2, Seq(s"$outName = ${localName(opName)}"), Seq(outName))
    } else if (isGlobal(context, opName)) {
      require(args.length == 0) // Typechecker should have ensured this
      require(ty == Number)

      val (context2, outName) = localAnon(context)
      (context2, Seq(s"$outName = ${globalName(opName)}"), Seq(outName))
    } else {

      val (context2, argLines, argNames) = translateArgs(context, args)
      val (context3, callLine, outNames) = ty match {
        case Number => {
          val (context3, outName) = localAnon(context2)
          (context3, s"$outName ${opcodeName(opName)} ${argNames.mkString(", ")}", Seq(outName))
        }

        case Component(inArity, outArity) => (
          context2,
          s"${outputs.mkString(", ")} ${opcodeName(opName)} ${(inputs ++ argNames).mkString(", ")}",
          outputs
        )

      }

      (context3, argLines ++ Seq(callLine), outNames)

    }
  }

  // Assign the arguments for an opcode call to local variables
  def translateArgs(context: Context, args: Seq[Expr]): (Context, CsLines, Seq[String]) =
    if (args.isEmpty) {
      (context, Seq(), Seq())
    } else {
      val (context2, argLines, headArgNames) = translateExpr(context, args.head)
      val (context3, argsLines, tailArgNames) = translateArgs(context2, args.tail)
      (context3, argLines ++ argsLines, headArgNames ++ tailArgNames)
    }
}
