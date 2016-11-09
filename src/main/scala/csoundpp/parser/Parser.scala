package cspp

import scala.language.postfixOps
import scala.util.parsing.combinator._
import scala.util.parsing.input.{Reader,Position,NoPosition}

class CsppTokenReader(tokens: Seq[CsppToken]) extends Reader[CsppToken] {
  override def first: CsppToken = tokens.head
  override def atEnd: Boolean = tokens.isEmpty
  override def pos: Position = tokens.headOption.map(_.pos).getOrElse(NoPosition)
  override def rest: Reader[CsppToken] = new CsppTokenReader(tokens.tail)
}

object CsppParser extends Parsers {

  override type Elem = CsppToken

  def apply(tokens: Seq[CsppToken]): Either[CsppParserError, Seq[Statement]] = {
    val reader = new CsppTokenReader(tokens)
    program(reader) match {
      case NoSuccess(msg, next) =>
        Left(new CsppParserError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, _) => Right(result)
    }
  }

  /**
   * <program> := <statement> <program>
   *            | <empty>
   */
  lazy val program: Parser[Seq[Statement]] = phrase(statement *)

  /**
   * <statement> := instr( <exprs> ) = <expr>
   *              | <ident> = <expr>
   *
   * <exprs> := <expr>
   *          | <expr>, <exprs>
   */
  lazy val statement: Parser[Statement] = positioned {
    ( (INSTR ~> LPAREN ~> rep1sep(expr, COMMA) <~ RPAREN) ~ (EQUALS ~> expr)
        ^^ { case n~v => Instrument(n, v) }
    | (id <~ EQUALS) ~ expr ^^ { case n~v => Assignment(n, v) }
    )
  }

  /**
   * <expr> := { <component>* }
   *         | <ident>
   *         | <number>
   */
  lazy val expr: Parser[Expr] = positioned {
    ( LBRACE ~> (component *) <~ RBRACE ^^ { case c => Chain(c) }
    | id                                ^^ { case i => Var(i) }
    | numberLiteral
    )
  }

  /**
   * <component> := <ident>(<exprs>)
   *              | <ident>
   */
  lazy val component: Parser[Component] = positioned {
    ( id ~ (LPAREN ~> repsep(expr, COMMA) <~ RPAREN) ^^ { case i~a => VarComponent(i, a) }
    | id                                             ^^ { case i => VarComponent(i, Seq()) }
    )
  }

  lazy val id: Parser[Ident] = positioned {
    accept("identifier", { case IDENT(name) => Ident(name) })
  }

  lazy val numberLiteral: Parser[Num] = positioned {
    accept("number literal", { case NUMBER(n) => Num(n) })
  }

}
