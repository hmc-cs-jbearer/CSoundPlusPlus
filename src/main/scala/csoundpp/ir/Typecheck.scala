package cspp

import scala.collection.immutable.HashMap
import scala.util.parsing.input.Positional

object CsppTypeChecker {

  /**
   * All variable bindings refer to functions. Even simple values, like x = 3, are considered
   * 0-argument functions. This makes the typecheck phase much, much simpler.
   */
  type Env = HashMap[Ident, Function]

  def apply(ast: Seq[Statement]): Either[CsppTypeError, Seq[Statement]] = apply(new Env(), ast)

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
      val newEnv = addVars(env, params, Seq.fill(params.length)(Function(Number, 0)))
      val annotated = annotateExpr(newEnv, expr)
      val ty = Function(typeOf(annotated), params.length)
      (addVar(env, id, ty), Assignment(id, params, annotated))
    }

    case Instrument(channels, expr) => {
      val annotatedChannels = channels.map(assertExpr(env, _: Expr, Number))
      val annotatedBody = assertExpr(env, expr, Source)
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
        expr.pos, "Expected " ++ expected.toString ++ " but found " ++ ty.toString ++ ".")
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
          comp.pos, "Expected source or effect, but found " ++ ty.toString ++ ".")
      }
    }
  }

  def assertComponent(env: Env, comp: Component, expected: CsppType): Component = {
    val annotated = annotateComponent(env, comp)
    val ty = typeOf(annotated)
    if (ty != expected) {
      throw new CsppTypeError(
        comp.pos, "Expected " ++ expected.toString ++ " but found " ++ ty.toString ++ ".")
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
    case None => throw new CsppTypeError(id.pos, "Unknown identifier '" ++ id.name ++ "'.")
  }

  def addVar(env: Env, id: Ident, ty: Function) =
    if (env contains id) {
      throw new CsppTypeError(
        id.pos, "Redeclaring identifier '" ++ id.name ++ "' with a different type.")
    } else {
      env + (id -> ty)
    }

  def addVars(env: Env, ids: Seq[Ident], tys: Seq[Function]): Env = {
    require(ids.length == tys.length)
    if (ids.isEmpty) {
      env
    } else {
      addVars(addVar(env, ids.head, tys.head), ids.tail, tys.tail)
    }
  }

  def typeOf[T <: TypeAnnotation with Positional](elem: T) = elem.ty match {
    case Some(ty) => ty
    case None => throw new CsppTypeError(
      elem.pos, "Unable to deduce type of " ++ elem.toString ++ ".")
  }

}
