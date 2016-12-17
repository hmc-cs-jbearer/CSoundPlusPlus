package cspp

import absyn._
import AbsynSugar._

trait Optimizers {

  type Optimization[T] = PartialFunction[T, T]

  var optimizations: Seq[Optimization[Statement]] = Seq()

  def stmtOptimization(xform: Optimization[Statement]) {
    optimizations = optimizations :+ xform
  }

  def exprOptimization(xform: Optimization[Expr]) = {

    def maybeXform(e: Expr) = if (xform isDefinedAt e) xform(e) else e

    def bottomUp(e: Expr): Expr = {
      e match {
        case n: Num => maybeXform(n)

        case b @ BinOp(l, _, r, _) => maybeXform(b.copy(left = bottomUp(l), right = bottomUp(r)))

        case c @ Chain(body, _) => maybeXform(c.copy(body = body map (bottomUp _)))

        case p @ Parallel(body, _) => maybeXform(p.copy(body = body map (bottomUp _)))

        case a @ Application(_, args, _) => maybeXform(a.copy(args = args map (bottomUp _)))
      }
    }

    def optimize(e: Expr) = e.ty match {
      case Some(ty) => bottomUp(e) annotated ty
      case None     => bottomUp(e)
    }

    stmtOptimization {
      case a @ Assignment(_, _, definition) => a copy (definition = optimize(definition))

      case i @ Instrument(channels, definition, None) =>
        i copy (channels = channels.map(optimize _), definition = optimize(definition))

      case s @ Instrument(channels, definition, Some(sends)) =>
        s copy (channels = channels.map(optimize _),
                definition = optimize(definition),
                sends = Some(optimize(sends)))
    }
  }

  def apply(ast: Seq[Statement]): Seq[Statement] = {
    var modified = ast
    for (xform <- optimizations) {
      modified = modified map { stmt =>
        if (xform isDefinedAt(stmt)) xform(stmt) else stmt
      }
    }
    return modified
  }
}

class CsppOptimizer extends Optimizers {

  // Constant folding
  exprOptimization {
    case b @ BinOp(l, op, r, _) => (l, r) match {
      case (Num(lVal, _), Num(rVal, _)) => op match {
        case Plus => Num(lVal + rVal)
        case Minus => Num(lVal - rVal)
        case Times => Num(lVal * rVal)
        case Divide => Num(lVal / rVal)
      }

      case _ => b
    }
  }

}

object CsppOptimizer {
  def apply(ast: Seq[Statement]): Either[CsppCompileError, Seq[Statement]] =
    Right(new CsppOptimizer()(ast))
}
