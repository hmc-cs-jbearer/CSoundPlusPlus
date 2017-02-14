package soundwave

import scala.collection.immutable.HashSet
import scala.util.parsing.input.Positional

import absyn._
import AbsynSugar._
import SwDag.Nodes._

object SwTranslator {

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

  var debug = false

  def apply(dag: Seq[StmtNode], debug: Boolean = false): Either[SwCompileError, CsLines] = {
    SwTranslator.debug = debug
    try {
      val (_, lines) = translateStmtNodes(EmptyContext, dag)
      Right(lines)
    } catch {
      case e: SwTranslateError => Left(e)
    }
  }

  // Get the csound type (i for init-rate, a for audio-rate, etc) of a soundwave expression
  def csType[T <: TypeAnnotation with SwPositional](elem: T) = elem.ty match {
    case Some(Number)                             => InitRate
    case Some(_)                                  => AudioRate
    case None                                     =>
      throw new SwTranslateError(elem.loc, s"Untyped expression ${elem}.")
  }

  def typeOf[T <: TypeAnnotation with SwPositional](elem: T): SwType = elem match {
    case CompNode(_, _, expr) => typeOf(expr)

    case _ => elem.ty match {
      case Some(ty) => ty
      case None =>
        throw new SwTranslateError(elem.loc, s"Untyped expression ${elem}.")
    }
  }

  // Generate an instrument name that is unique to the given context
  def instrName(context: Context) = {
    val id = context.instrs
    (context.copy(instrs = id + 1), id.toString)
  }

  // User-defined opcode names are mangled to avoid clashes with CSound built-in opcodes
  def opcodeName(id: Ident) = id match {
    case Ident(name) => s"_$name"
  }

  def localName(id: Ident) = s"i${id.name}"

  def globalName(id: Ident) = s"gi${id.name}"

  // Generate a local variable name that is unique to the given context
  def localAnon(context: Context) = {
    val id = context.locals
    (extendLocalScope(context.copy(locals = id + 1), id.toString), s"i$id")
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

  // Generate a CSound prints statement
  def log(level: LogLevel, name: String, fmt: String, params: String*): CsLines =
    if (debug) {
      val fmtStr = "\"" ++ s"SWLOG $level name=$name $fmt" ++ "\\n\""
      Seq("prints " ++ (fmtStr +: params).mkString(", "))
    } else {
      Seq()
    }

  /**
   * Create a new user defined opcode which implements a SoundWave component. The opcode may have one
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
   * Create a new user defined opcode which implemets a SoundWave scalar-valued function. All inputs
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
    body ++ Seq(
    s"chout $outVar, $name", // Buffer output so other instruments can read it via channel
    s"out $outVar",
    "endin",
    "")) // End with a blank line just for readability

  // Translate sends to an always on instrument which reads input from the corresponding channel
  def sendsDefine(name: String, channel: CsLines, channelVar: String, opcode: String) =
    Seq(
    s"instr ${name}") ++
    channel ++ Seq(
    s"a0 channel $channelVar",
    s"a1 $opcode a0",
    "out a1",
    "endin",
    "",
    // Make sure instrument is always on, ready to receive input
    s"; instr $name is always on; it monitors channel $channelVar for output",
    s"turnon $name",
    ""
    )

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
      val context2 = extendLocalScope(context, params)

      val name = opcodeName(id)
      val paramNames = params.map(_.name)

      val (context3, body, scalarOutputs) = translateExpr(context2, expr)

      val (context4, opcodeLines) = typeOf(expr) match {
        case _: Component => (context3, component(name, inputs, paramNames, body, outputs))
        case _            => {
          // Csound will not let us "xout" a constant, we have to wrap it in an irate variable
          if (Seq('i', 'k') contains scalarOutputs.head(0)) {
            (context3, function(name, paramNames, body, scalarOutputs.head))
          } else {
            val (context4, outVar) = localAnon(context3)
            (context4, function(name, paramNames, body :+ s"$outVar = ${scalarOutputs.head}", outVar))
          }
        }
      }

      // We return the original context, because any locals defined in this body go out of scope
      // at the end of the opcode definition. Thus, we can reuse their names in the next statement.
      (context, opcodeLines)
    }

    case InstrNode(inputs, outputs, Instrument(channels, expr, sends)) => {
      require(inputs.length == 0)
      require(outputs.length == 1)
      val outName = outputs.head

      val (context2, instrId) = instrName(context)
      val localContext = extendLocalScope(context2, Seq(Ident("freq"), Ident("amp")))

      // We ignore the final context for the same reason as above
      val (_, body, _) = translateExpr(localContext, expr)

      val instrLines = instrDefine(instrId, body, outName)
      val (context3, channelLines) = translateChannels(context2, instrId, channels)

      val (context4, sendsLines) = sends match {
        case None => (context3, Seq())
        case Some(s) => translateSends(context3, instrId, channels, s)
      }

      (context4, instrLines ++ channelLines ++ sendsLines)
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

  // Generate CSound code to create a sends instrument for each channel
  def translateSends(context: Context, instr: String, channels: Seq[Expr], sends: Expr): (Context, CsLines) = {
    val (_, bodyLines, outputs) = translateExpr(context, sends)

    val inputs = sends match {
      case CompNode(in, _, _) => in
      case _ => throw new SwTranslateError(
        sends.loc, "Sends expression was not translated to DAG node.")
    }

    val opcodeLines = s"; Sends effects for instrument $instr" +:
      component(s"sends$instr", inputs, Seq(), bodyLines, outputs)

    val (context2, instrLines) = translateSends(context, channels, s"sends$instr")

    (context2, opcodeLines ++ instrLines)
  }

  def translateSends(context: Context, channels: Seq[Expr], sendsInstr: String): (Context, CsLines) =
    if (channels.isEmpty) {
      (context, Seq())
    } else {
      val (context2, channelLines, channelVars) = translateExpr(context, channels.head)
      require(channelVars.length == 1)
      val channelVar = channelVars.head

      val (context3, newInstr) = instrName(context2)
      val headLines = sendsDefine(newInstr, channelLines, channelVar, sendsInstr)
      val (context4, tailLines) = translateSends(context3, channels.tail, sendsInstr)
      (context4, headLines ++ tailLines)
    }

  /**
   * Generate code to store the value of expr in one or more local variables.
   * This function simply allocates any necessary local variables to store the output and delegates
   * to the overload below.
   */
  def translateExpr(context: Context, expr: Expr): (Context, CsLines, Seq[String]) = expr match {
    case Num(n, _) => {
      // You can just use the literal n in place of a variable containing the value n
      (context, Seq(), Seq(n.toString))
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
      throw new SwTranslateError(expr.loc, "Chain expression was not transformed to DAG node.")

    case p: Parallel =>
      throw new SwTranslateError(expr.loc, "Parallel expression was not transformed to DAG node.")

    case _: Let => throw new SwTranslateError(expr.loc, "Unresolved let.")
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
      (context, Seq(), Seq(localName(opName)))
    } else if (isGlobal(context, opName)) {
      require(args.length == 0) // Typechecker should have ensured this
      require(ty == Number)
      (context, Seq(), Seq(globalName(opName)))
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