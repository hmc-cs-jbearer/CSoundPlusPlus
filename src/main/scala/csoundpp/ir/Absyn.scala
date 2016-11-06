package cspp

import scala.util.parsing.input.Positional

/**
 * Used to store positional information to aid in error reporting.
 */
trait ASTElem extends Positional

/**
 * <expr> := <chain> | <ident> | <number>
 * All expressions are annotated with an optional type. The type annotation is
 * initialized to None at parse time and then filled in in the typecheck phase.
 */
abstract class Expr extends ASTElem
case class Chain(body: Seq[Component], ty: Option[CSType] = None) extends Expr
case class Var(name: Ident, ty: Option[CSType] = None) extends Expr
case class Num(value: Double, ty: Option[CSType] = None) extends Expr

/**
 * <statement> := <ident> = <expr>
 *              | instr( <expr>* ) = <expr>
 */
abstract class Statement extends ASTElem
case class Assignment(name: Ident, value: Expr) extends Statement
case class Instrument(channels: Seq[Expr], definition: Expr) extends Statement

/**
 * <component> := <ident>
 *              | <ident>( <expr>* )
 */
abstract class Component extends ASTElem
case class VarComponent(name: Ident, args: Seq[Expr]) extends Component

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
abstract class CSType
object Source extends CSType
object Effect extends CSType
object Number extends CSType
