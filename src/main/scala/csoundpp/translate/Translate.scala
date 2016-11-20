package cspp

import scala.collection.immutable.HashSet
import scala.util.parsing.input.Positional

import absyn._

abstract class CsType(val typeId: Char) {
  override def toString = typeId.toString
}

object InitRate extends CsType('i')
object AudioRate extends CsType('a')

// Keeps track of how many things (local variables etc) have been created.
// This allows us to always generate unique names.
case class Context(locals: Int, instrs: Int, localScope: HashSet[Ident], globalScope: HashSet[Ident])

// instr 0 is reserved in CSound, so we have to start out with 1 instrument
object emptyContext extends Context(0, 1, HashSet(), HashSet())

object CsppTranslator {

  type CsLine = String
  type CsLines = Seq[CsLine]

  val sigName = "asig"

  def apply(ast: Seq[Statement]): Either[CsppCompileError, CsLines] = {
    try {
      val (_, lines) = translateStmts(emptyContext, ast)
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

  def typeOf[T <: TypeAnnotation with CsppPositional](elem: T) = elem.ty match {
    case Some(Function(ty, _)) => ty
    case Some(ty) => ty
    case None =>
      throw new CsppTranslateError(elem.loc, s"Untyped expression ${elem}.")
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

  // Create a new user defined opcode
  def opcodeDefine(name: String, params: Seq[String], body: CsLines, ty: CsppType, resultTy: CsType, outVar: String): CsLines = {
    val allParams = if (ty == Effect) {
      // Effects have an extra input parameter, the arate signal to modify
      sigName +: params.map(localName(_))
    } else {
      params.map(localName(_))
    }

    val (paramTypes, xin) = if (allParams.isEmpty) {
      ("0", // CSound's way of saying there are no input params
       "")
    } else {
      // The type of the params is the first character of their name
      (allParams.map((param: String) => param(0)).mkString,
       allParams.mkString(", ") ++ " xin")
    }

    s"opcode ${name}, ${resultTy}, $paramTypes" +:
    xin +:
    (body ++ Seq(
    s"xout $outVar",
    "endop",
    "")) // End with a blank line just for readability
  }

  // Create a new instrument
  def instrDefine(name: String, body: CsLines, outVar: String): CsLines =
    s"instr ${name}" +:
    "iamp ampmidi 1" +:
    "ifreq cpsmidi" +:
    (body ++ Seq(
    s"out $outVar",
    "endin",
    "")) // End with a blank line just for readability

  def translateStmts(context: Context, stmts: Seq[Statement]): (Context, CsLines) =
    if (stmts.isEmpty) {
      (context, Seq())
    } else {
      val (context2, headLines) = translateStmt(context, stmts.head)
      val (context3, tailLines) = translateStmts(context2, stmts.tail)
      (context3, headLines ++ tailLines)
    }

  def translateStmt(context: Context, stmt: Statement): (Context, CsLines) = stmt match {
    case Assignment(id, params, expr) => {
      val localContext = extendLocalScope(context, params)

      // We ignore the context we get out, because any locals defined in this body go out of scope
      // at the end of the opcode definition. Thus, we can reuse their names in the next statement.
      val (_, body, outName) = translateExpr(localContext, expr)
      (context, opcodeDefine(
        opcodeName(id), params.map((param: Ident) => param.name), body, typeOf(expr), csType(expr), outName))
    }

    case Instrument(channels, expr) => {
      val (context2, instrId) = instrName(context)
      val localContext = extendLocalScope(context2, Seq(Ident("freq"), Ident("amp")))

      // We ignore the final context for the same reason as above
      val (_, body, outName) = translateExpr(localContext, expr)

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
      val (context2, channelLines, channelVar) = translateExpr(context, channels.head)
      val mapLines = channelLines ++ Seq(s"massign $channelVar, $instrId")
      val (context3, restLines) = translateChannels(context2, instrId, channels.tail)
      (context3, mapLines ++ restLines)
    }
  }

  // Generate code to store the value of expr in a local variable.
  def translateExpr(context: Context, expr: Expr): (Context, CsLines, String) = expr match {
    case Num(n, _) => {
      val (context2, localVar) = localAnon(context)
      (context2, Seq(s"$localVar = $n"), localVar)
    }

    case BinOp(l, op, r, _) => {
      val (context2, leftLines, leftVar) = translateExpr(context, l)
      val (context3, rightLines, rightVar) = translateExpr(context2, r)
      val (context4, localVar) = localAnon(context3)
      val opStr = op match {
        case Plus => "+"
        case Minus => "-"
        case Times => "*"
        case Divide => "/"
      }
      (context4, leftLines ++ rightLines :+ s"$localVar = $leftVar $opStr $rightVar", localVar)
    }

    case app: Application => opcodeCall(context, app)

    case Chain(comps, Some(Effect)) => if (comps.isEmpty) {
      // An empty effect just leaves asig alone
      (context, Seq(""), sigName)
    } else {
      val (context2, lines) = translateComps(context, comps)
      (context2, lines, sigName)
    }

    case Chain(comps, Some(Source)) => if (comps.isEmpty) {
      // An empty source produces no signal
      (context, Seq("$sigName = 0"), sigName)
    } else {
      val (context2, lines) = translateComps(context, comps)
      (context2, lines, sigName)
    }
  }

  def translateComps(context: Context, comps: Seq[Expr]): (Context, CsLines) = {
    require(!comps.isEmpty)

    // The typechecker has already verified that the app is of the correct type (either source
    // or effect). So when we translate the expression, we'll get something like
    //    asig opcode args...
    // or
    //    asig opcode asig args...
    // Which automatically takes care of the plumbing for us
    val (context2, compLines, _) = translateExpr(context, comps.head)

    if (comps.length == 1) {
      (context2, compLines)
    } else {
      val (context3, compsLines) = translateComps(context2, comps.tail)
      (context3, compLines ++ compsLines)
    }
  }

  // Call an opcode with the given args, returning the name of the output variable
  def opcodeCall(context: Context, app: Application): (Context, CsLines, String) = {
    val Application(Ident(opName), args, Some(ty)) = app

    if (isLocal(context, opName)) {
      require(args.length == 0) // Typechecker should have ensured this
      require(ty == Number)

      val (context2, outName) = localAnon(context)
      (context2, Seq(s"$outName = ${localName(opName)}"), outName)
    } else if (isGlobal(context, opName)) {
      require(args.length == 0) // Typechecker should have ensured this
      require(ty == Number)

      val (context2, outName) = localAnon(context)
      (context2, Seq(s"$outName = ${globalName(opName)}"), outName)
    } else {

      val (context2, argLines, argNames) = translateArgs(context, args)
      val (context3, callLine, outName) = ty match {
        case Number => {
          val (context3, outName) = localAnon(context2)
          (context3, s"$outName ${opcodeName(opName)} ${argNames.mkString(", ")}", outName)
        }

        case Source =>
          (context2, s"$sigName ${opcodeName(opName)} ${argNames.mkString(", ")}", sigName)


        case Effect =>
          (context2, s"$sigName ${opcodeName(opName)} ${(sigName +: argNames).mkString(", ")}", sigName)
      }

      (context3, argLines ++ Seq(callLine), outName)

    }
  }

  // Assign the arguments for an opcode call to local variables
  def translateArgs(context: Context, args: Seq[Expr]): (Context, CsLines, Seq[String]) =
    if (args.isEmpty) {
      (context, Seq(), Seq())
    } else {
      val (context2, argLines, argName) = translateExpr(context, args.head)
      val (context3, argsLines, argNames) = translateArgs(context2, args.tail)
      (context3, argLines ++ argsLines, argName +: argNames)
    }
}
