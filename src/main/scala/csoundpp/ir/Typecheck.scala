package cspp

import scala.collection.immutable.HashMap
import scala.util.parsing.input.Positional

object CsppTypeChecker {

  type Env = HashMap[Ident, CsppType]

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
    case Assignment(id, expr) => {
      val annotated = annotateExpr(env, expr)
      (addVar(env, id, typeOf(annotated)), Assignment(id, annotated))
    }
    case Instrument(channels, expr) => {
      val annotatedChannels = channels.map(assertExpr(env, _: Expr, Number))
      val annotatedBody = assertExpr(env, expr, Source)
      (env, Instrument(annotatedChannels, annotatedBody))
    }
  }

  def annotateExpr(env: Env, expr: Expr): Expr = expr match {
    case Num(n, _)  => Num(n, Some(Number))
    case Var(id, _) => Var(id, Some(lookupVar(env, id)))
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
    case VarComponent(id, args, _) => {
      val annotatedArgs = args.map(assertExpr(env, _, Number))
      lookupVar(env, id) match {
        case Source => VarComponent(id, annotatedArgs, Some(Source))
        case Effect => VarComponent(id, annotatedArgs, Some(Effect))
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

  def lookupVar(env: Env, id: Ident) = env get id match {
    case Some(ty) => ty
    case None => throw new CsppTypeError(id.pos, "Unknown identifier '" ++ id.name ++ "'.")
  }

  def addVar(env: Env, id: Ident, ty: CsppType) =
    if (env contains id) {
      throw new CsppTypeError(
        id.pos, "Redeclaring identifier '" ++ id.name ++ "' with a different type.")
    } else {
      env + (id -> ty)
    }

  def typeOf[T <: TypeAnnotation with Positional](elem: T) = elem.ty match {
    case Some(ty) => ty
    case None => throw new CsppTypeError(
      elem.pos, "Unable to deduce type of " ++ elem.toString ++ ".")
  }

}
