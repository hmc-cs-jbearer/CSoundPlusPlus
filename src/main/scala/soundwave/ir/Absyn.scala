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
  def unannotatedString: String
  override def toString = unannotatedString ++
    (ty match {
      case Some(t) => s" [$t]"
      case None    => ""
    })
}

case class Chain(body: Seq[Expr], ty: Option[SwType] = None) extends Expr {
  def annotated(newTy: SwType) = Chain(body, Some(newTy))
  def unannotatedString = s"{ ${body.mkString(" ")} }"
}

case class Parallel(body: Seq[Expr], ty: Option[SwType] = None)
  extends Expr
{
  def annotated(newTy: SwType) = Parallel(body, Some(newTy))
  def unannotatedString = s"parallel {${body.mkString(" ")}}"
}

case class Application(name: Ident, args: Seq[Expr], ty: Option[SwType] = None) extends Expr {
  def annotated(newTy: SwType) = Application(name, args, Some(newTy))
  def unannotatedString = s"$name(${args.mkString(", ")})"
}

case class BinOp(left: Expr, op: Bop, right: Expr, ty: Option[SwType] = None)
  extends Expr
{
  def annotated(newTy: SwType) = BinOp(left, op, right, Some(newTy))
  def unannotatedString = s"($left $op $right)"
}

case class Num(value: Double, ty: Option[SwType] = None) extends Expr {
  def annotated(newTy: SwType) = Num(value, Some(newTy))
  def unannotatedString = value.toString
}

case class Let(bindings: Seq[Assignment], body: Expr, ty: Option[SwType] = None) extends Expr {
  def annotated(newTy: SwType) = Let(bindings, body, Some(newTy))
  def unannotatedString = s"let {${bindings.mkString(" ")}} in $body"
}

abstract class Bop(val lexeme: String) {
  override def toString = lexeme
}
case object Plus extends Bop("+")
case object Minus extends Bop("-")
case object Times extends Bop("*")
case object Divide extends Bop("/")

/**
 * <statement> := <ident> = <expr>
 *              | instr( <expr>* ) = <expr>
 */
abstract class Statement extends ASTElem
case class Assignment(name: Ident, params: Seq[Ident], definition: Expr) extends Statement {
  override def toString = s"$name(${params.mkString(", ")}) = $definition"
}
case class Instrument(channels: Seq[Expr], definition: Expr, sends: Option[Expr] = None)
  extends Statement
{
  override def toString = s"instr(${channels.mkString(", ")}) = $definition"
}

/**
 * Identifiers must start with a letter, and can contain letters, numbers, and
 * underscores.
 */
case class Ident(name: String) extends ASTElem {
  override def toString = name
}

/**
 * The SoundWave type system supports 3 types:
 * source: a chain with no inputs and one output
 * effect: a chain with one input and one output
 * number: a floating point constant used for parameterization of chains
 */
abstract class SwType
case object Number extends SwType {
  override def toString = "number"
}
// All sources, effects, and multiplexers are components. Sources and effects are really just
// special multiplexers with arity 0 and 1, respectively.
case class Component(inArity: Int, outArity: Int) extends SwType {
  override def toString = s"component ($inArity -> $outArity)"
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// Syntactic sugar
////////////////////////////////////////////////////////////////////////////////////////////////////

object AbsynSugar {
  implicit def string2Ident(s: String) = Ident(s)

  implicit def double2Num(d: Double) = Num(d)

  // Source is an alias for a component with no input and one output
  object Source extends Component(0, 1)

  // Effect is an alias for a component with one input and one output
  object Effect extends Component(1, 1)
}
