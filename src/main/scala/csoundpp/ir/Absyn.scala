package cspp.absyn

import scala.language.implicitConversions
import scala.util.parsing.input.Positional

import cspp._

/**
 * Used to store positional information to aid in error reporting.
 */
trait ASTElem extends CsppPositional

abstract trait TypeAnnotation {
  val ty: Option[CsppType]
}

/**
 * <expr> := <chain> | <ident> | <arith_expr>
 * All expressions are annotated with an optional type. The type annotation is
 * initialized to None at parse time and then filled in in the typecheck phase.
 */
abstract class Expr extends ASTElem with TypeAnnotation {
  def annotated(newTy: CsppType): Expr
}
case class Chain(body: Seq[Expr], ty: Option[CsppType] = None) extends Expr {
  def annotated(newTy: CsppType) = Chain(body, Some(newTy))
}
case class Application(name: Ident, args: Seq[Expr], ty: Option[CsppType] = None) extends Expr {
  def annotated(newTy: CsppType) = Application(name, args, Some(newTy))
}

case class BinOp(left: Expr, op: Bop, right: Expr, ty: Option[CsppType] = None)
  extends Expr
{
  def annotated(newTy: CsppType) = BinOp(left, op, right, Some(newTy))
}

case class Num(value: Double, ty: Option[CsppType] = None) extends Expr {
  def annotated(newTy: CsppType) = Num(value, Some(newTy))
}

abstract class Bop
case object Plus extends Bop
case object Minus extends Bop
case object Times extends Bop
case object Divide extends Bop

/**
 * <statement> := <ident> = <expr>
 *              | instr( <expr>* ) = <expr>
 */
abstract class Statement extends ASTElem
case class Assignment(name: Ident, params: Seq[Ident], definition: Expr) extends Statement
case class Instrument(channels: Seq[Expr], definition: Expr) extends Statement

/**
 * Identifiers must start with a letter, and can contain letters, numbers, and
 * underscores.
 */
case class Ident(name: String) extends ASTElem

/**
 * The CSound++ type system supports 3 types:
 * source: a chain with no inputs and one output
 * effect: a chain with one input and one output
 * number: a floating point constant used for parameterization of chains
 */
abstract class CsppType
case object Source extends CsppType
case object Effect extends CsppType
case object Number extends CsppType
case class Function(resultTy: CsppType, arity: Int) extends CsppType

////////////////////////////////////////////////////////////////////////////////////////////////////
// Syntactic sugar
////////////////////////////////////////////////////////////////////////////////////////////////////

object Ident {
    implicit def String2Ident(s: String) = Ident(s)
}
