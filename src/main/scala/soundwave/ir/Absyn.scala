package soundwave.absyn

import scala.language.implicitConversions
import scala.util.parsing.input.Positional

import soundwave._

/**
 * Used to store positional information to aid in error reporting.
 */
trait ASTElem extends SwPositional

abstract trait TypeAnnotation {
  val ty: Option[SwType]
}

/**
 * <expr> := <mux> | <chain> | <application> | <arith_expr>
 * All expressions are annotated with an optional type. The type annotation is
 * initialized to None at parse time and then filled in in the typecheck phase.
 */
abstract class Expr extends ASTElem with TypeAnnotation {
  def annotated(newTy: SwType): Expr
}

case class Chain(body: Seq[Expr], ty: Option[SwType] = None) extends Expr {
  def annotated(newTy: SwType) = Chain(body, Some(newTy))
}

case class Parallel(body: Seq[Expr], ty: Option[SwType] = None)
  extends Expr
{
  def annotated(newTy: SwType) = Parallel(body, Some(newTy))
}

case class Application(name: Ident, args: Seq[Expr], ty: Option[SwType] = None) extends Expr {
  def annotated(newTy: SwType) = Application(name, args, Some(newTy))
}

case class BinOp(left: Expr, op: Bop, right: Expr, ty: Option[SwType] = None)
  extends Expr
{
  def annotated(newTy: SwType) = BinOp(left, op, right, Some(newTy))
}

case class Num(value: Double, ty: Option[SwType] = None) extends Expr {
  def annotated(newTy: SwType) = Num(value, Some(newTy))
}

case class Let(bindings: Seq[Assignment], body: Expr, ty: Option[SwType] = None) extends Expr {
  def annotated(newTy: SwType) = Let(bindings, body, Some(newTy))
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
case class Instrument(channels: Seq[Expr], definition: Expr, sends: Option[Expr] = None) extends Statement

/**
 * Identifiers must start with a letter, and can contain letters, numbers, and
 * underscores.
 */
case class Ident(name: String) extends ASTElem

/**
 * The SoundWave type system supports 3 types:
 * source: a chain with no inputs and one output
 * effect: a chain with one input and one output
 * number: a floating point constant used for parameterization of chains
 */
abstract class SwType
case object Number extends SwType
case class Function(resultTy: SwType, arity: Int) extends SwType

// All sources, effects, and multiplexers are components. Sources and effects are really just
// special multiplexers with arity 0 and 1, respectively.
case class Component(inArity: Int, outArity: Int) extends SwType

////////////////////////////////////////////////////////////////////////////////////////////////////
// Syntactic sugar
////////////////////////////////////////////////////////////////////////////////////////////////////

object AbsynSugar {
  implicit def string2Ident(s: String) = Ident(s)

  // Source is an alias for a component with no input and one output
  object Source extends Component(0, 1)

  // Effect is an alias for a component with one input and one output
  object Effect extends Component(1, 1)
}
