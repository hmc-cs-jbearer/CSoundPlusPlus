package cspp

import scala.collection.immutable.HashMap
import scala.util.parsing.input.Positional

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
      Ident("fm") -> Function(Source, 3),
      Ident("compress") -> Function(Effect, 2)
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
      val annotatedChannels = channels.map(assertExpr(env, _: Expr, Number))

      // Bind MIDI params (freq, amp, etc) to Numbers in the body of the instrument
      val paramTy = Function(Number, 0)
      val localEnv = addVars(env,
        Ident("freq") -> paramTy,
        Ident("amp") -> paramTy
      )

      val annotatedBody = assertExpr(localEnv, expr, Source)

      // We return the old env, because the new identifiers are only in scope within the instrument
      (env, Instrument(annotatedChannels, annotatedBody))
    }

  }

  def annotateExpr(env: Env, expr: Expr): Expr = expr match {
    case n: Num => n annotated Number

    case app @ Application(id, args, _) => annotateApp(env, app)

    case Chain(body, _) => {
      // The type of a chain is either source or effect, based on the first component in the chain.
      val head = annotateComponent(env, body.head)
      val ty = typeOf(head)

      // Regardless of whether a chain is a source or effect, everything after the first component
      // must be an effect.
      val tail = body.tail.map(assertComponent(env, _: Component, Effect))

      Chain(head +: tail, Some(ty))
    }
  }

  def assertExpr(env: Env, expr: Expr, expected: CsppType): Expr = {
    val annotated = annotateExpr(env, expr)
    val ty = typeOf(annotated)
    if (ty != expected){
      throw new CsppTypeError(
        expr.pos, s"Expected ${expected.toString} but found ${ty.toString}.")
    } else {
      annotated
    }
  }

  def annotateComponent(env: Env, comp: Component): Component = comp match {
    case AppComponent(app, _) => {
      val annotatedApp = annotateApp(env, app)
      typeOf(annotatedApp) match {
        case Source => AppComponent(annotatedApp) annotated Source
        case Effect => AppComponent(annotatedApp) annotated Effect
        case ty     => throw new CsppTypeError(
          comp.pos, s"Expected source or effect, but found ${ty.toString}.")
      }
    }
  }

  def assertComponent(env: Env, comp: Component, expected: CsppType): Component = {
    val annotated = annotateComponent(env, comp)
    val ty = typeOf(annotated)
    if (ty != expected) {
      throw new CsppTypeError(
        comp.pos, s"Expected ${expected.toString} but found ${ty.toString}.")
    } else {
      annotated
    }
  }

  def annotateApp(env: Env, app: Application): Application = {
    val id = app.name
    val args = app.args
    val annotatedArgs = args.map(assertExpr(env, _, Number))
    val func = lookupVar(env, id)
    if (func.arity == args.length) {
      Application(id, annotatedArgs) annotated func.resultTy
    } else {
      throw new CsppTypeError(app.pos,
        s"Wrong number of arguments to callable '${id}'. " ++
        s"Expected ${func.arity}, found ${args.length}.")
    }
  }

  def lookupVar(env: Env, id: Ident) = env get id match {
    case Some(ty) => ty
    case None => throw new CsppTypeError(id.pos, s"Unknown identifier '${id.name}'.")
  }

  def addVars(env: Env, mappings: (Ident, Function)*): Env = {
    if (mappings.isEmpty) {
      env
    } else if (env contains mappings.head._1) {
      val pos = mappings.head._1.pos
      val name = mappings.head._1.name
      throw new CsppTypeError(
        pos, s"Redeclaring identifier '$name' with a different type.")
    } else {
      addVars(env + mappings.head, mappings.tail: _*)
    }
  }

  def typeOf[T <: TypeAnnotation with Positional](elem: T) = elem.ty match {
    case Some(ty) => ty
    case None => throw new CsppTypeError(
      elem.pos, s"Unable to deduce type of $elem.")
  }

}
