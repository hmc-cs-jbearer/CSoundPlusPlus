package cspp

import scala.collection.immutable.{HashMap,HashSet}
import scala.util.parsing.input.Positional

import absyn._
import AbsynSugar._

object CsppTypeChecker {

  /**
   * All variable bindings refer to functions. Even simple values, like x = 3, are considered
   * 0-argument functions. This makes the typecheck phase much, much simpler.
   */
  type Env = HashMap[Ident, Function]

  /**
   * Typecheck the given program. Typechecking is performed in an environment where built-in
   * components are already mapped to their proper type. For example, fm -> Function(Source, 3) and
   * compress -> Function(Effect, 2)
   */
  def apply(ast: Seq[Statement]): Either[CsppTypeError, Seq[Statement]] = apply(
    addVars(new Env(),
      Ident("foscil") -> Function(Source, 4),
      Ident("sine") -> Function(Source, 2),
      Ident("sidechain_compress") -> Function(Component(2, 1), 5)
      Ident("compress") -> Function(Effect, 5),
      Ident("adsr") -> Function(Effect, 4)
    ),
  ast)

  // Used for unit tests that want to specify their own set of built-in bindings
  def apply(env: Env, ast: Seq[Statement]): Either[CsppTypeError, Seq[Statement]] = {
    try {
      Right(annotateStmts(env, ast))
    } catch {
      case e: CsppTypeError => Left(e)
    }
  }

  def annotateStmts(env: Env, stmts: Seq[Statement]): Seq[Statement] =
    if (stmts.isEmpty) {
      Seq()
    } else {
      val (newEnv, annotated) = annotateStmt(env, stmts.head)
      annotated +: annotateStmts(newEnv, stmts.tail)
    }

  def annotateStmt(env: Env, stmt: Statement): (Env, Statement) = stmt match {

    case Assignment(id, params, expr) => {
      // The params are in scope within the definition, so we have to annotate expr with an
      // extended environment which accounts for this
      val newBindings = for {
        param <- params
      } yield param -> Function(Number, 0)
      val newEnv = addVars(env, newBindings: _*)
      val annotated = annotateExpr(newEnv, expr)
      val ty = Function(typeOf(annotated), params.length)
      (addVars(env, id -> ty), Assignment(id, params, annotated))
    }

    case Instrument(channels, expr) => {
      val annotatedChannels = channels.map(
        assertExpr(env, _: Expr, "number", { case Number => () })._1)

      // Bind MIDI params (freq, amp, etc) to Numbers in the body of the instrument
      val paramTy = Function(Number, 0)
      val localEnv = addVars(env,
        Ident("freq") -> paramTy,
        Ident("amp") -> paramTy
      )

      val (annotatedBody, _) = assertExpr(localEnv, expr, "source", { case Source => () })

      // We return the old env, because the new identifiers are only in scope within the instrument
      (env, Instrument(annotatedChannels, annotatedBody))
    }

  }

  def annotateExpr(env: Env, expr: Expr): Expr = expr match {
    case n: Num => n annotated Number

    case BinOp(l, op, r, _) => {
      val (annotatedLeft, _) = assertExpr(env, l, "number", { case Number => () })
      val (annotatedRight, _) = assertExpr(env, r, "number", { case Number => () })
      BinOp(annotatedLeft, op, annotatedRight) annotated Number
    }

    case app @ Application(id, args, _) => annotateApp(env, app)

    case Chain(body, _) =>
      if (body.isEmpty) {
        // An empty chain is the boring effect that passes its input through unchanged.
        Chain(body) annotated Effect
      } else {
        val (head, (inArity, headOutArity)) =
          assertExpr(env, body.head, "component", { case Component(in, out) => (in, out) })

        val tail = assertComponents(env, body.tail, headOutArity)

        val (_, outArity) = assertExpr(env, (head +: tail).last, "component",
                                       { case Component(_, out) => out })

        Chain(head +: tail) annotated Component(inArity, outArity)
    }

    case Parallel(body, _) => {
      // Annotate the inputs, and compute the arity
      val (annotatedInputs, arities) =
        body.map(assertExpr(env, _, "component", { case Component(in, out) => (in, out) })).unzip

      val (inArities, outArities) = arities.unzip

      val (inArity, outArity) = if (body.isEmpty) {
        // An empty parallel block is treated the same as an empty chain: a trivial effect
        (1, 1)
      } else {
        (inArities.sum, outArities.sum)
      }

      Parallel(annotatedInputs) annotated Component(inArity, outArity)
    }
  }

  def assertComponents(env: Env, body: Seq[Expr], inArity: Int): Seq[Expr] =
    if (body.isEmpty) {
      Seq()
    } else {
      val (head, headOutArity) = assertExpr(
        env, body.head, s"component with $inArity inputs",
        { case Component(in, out) if in == inArity => out })

      val tail = assertComponents(env, body.tail, headOutArity)

      head +: tail
    }

  def assertExpr[T](env: Env, expr: Expr, expected: String, f: PartialFunction[CsppType, T]) = {
    val annotated = annotateExpr(env, expr)
    val ty = typeOf(annotated)
    if (f isDefinedAt ty) {
      (annotated, f(ty))
    } else {
      throw new CsppTypeError(
        expr.loc, s"Expected ${expected} but found ${ty}.")
    }
  }

  def annotateApp(env: Env, app: Application): Application = {
    val id = app.name
    val args = app.args
    val annotatedArgs = args.map(assertExpr(env, _, "number", { case Number => () })._1)
    val func = lookupVar(env, id)
    if (func.arity == args.length) {
      Application(id, annotatedArgs) annotated func.resultTy
    } else {
      throw new CsppTypeError(app.loc,
        s"Wrong number of arguments to callable '${id}'. " ++
        s"Expected ${func.arity}, found ${args.length}.")
    }
  }

  def assertApp(env: Env, app: Application, expected: CsppType): Application = {
    val annotated = annotateApp(env, app)
    val ty = typeOf(annotated)
    if (ty != expected) {
      throw new CsppTypeError(
        app.loc, s"Expected ${expected.toString} but found ${ty.toString}.")
    } else {
      annotated
    }
  }

  def lookupVar(env: Env, id: Ident) = env get id match {
    case Some(ty) => ty
    case None => throw new CsppTypeError(id.loc, s"Unknown identifier '${id.name}'.")
  }

  def addVars(env: Env, mappings: (Ident, Function)*): Env = {
    if (mappings.isEmpty) {
      env
    } else if (env contains mappings.head._1) {
      val pos = mappings.head._1.loc
      val name = mappings.head._1.name
      val orig = env.keys.filter(_ == mappings.head._1).head.loc
      val msg = if (orig == NoLocation)
        s"Redefinition of built-in '$name'."
      else
        s"Redefinition of '$name'. First declared here:\n$orig"
      throw new CsppTypeError(pos, msg)
    } else {
      addVars(env + mappings.head, mappings.tail: _*)
    }
  }

  def typeOf[T <: TypeAnnotation with CsppPositional](elem: T) = elem.ty match {
    case Some(ty) => ty
    case None => throw new CsppTypeError(
      elem.loc, s"Unable to deduce type of $elem.")
  }

  implicit class TypeSetBuilder(input: CsppType) {
    def ||(other: CsppType) = new HashSet + (input, other)
  }

  implicit class TypeSetExtender(input: Set[CsppType]) {
    def ||(other: CsppType) = input + other
  }

}
