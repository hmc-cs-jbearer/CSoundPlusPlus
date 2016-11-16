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

class CsppParser extends Parsers {

  override type Elem = CsppToken

  def run(tokens: Seq[CsppToken]): Either[CsppCompileError, Seq[Statement]] = {
    var reader = new CsppTokenReader(tokens)
    phrase(program)(reader) match {
      case NoSuccess(msg, next) =>
        Left(new CsppParserError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, _) => Right(result)
    }
  }

  /**
   * <program> := <statement> <program>
   *            | <empty>
   */
  def program: Parser[Seq[Statement]] = phrase(statement *)

  /**
   * <statement> := instr( <exprs> ) = <expr>
   *              | <ident>( <idents> ) = <expr>
   *              | <ident> = <expr>
   *
   * <exprs> := <expr>
   *          | <expr>, <exprs>
   *
   * <idents> := <empty>
   *           | <ident>, <idents>
   */
  def statement: Parser[Statement] = positioned {
    ( (INSTR ~> LPAREN ~> rep1sep(expr, COMMA) <~ RPAREN) ~ (EQUALS ~> expr)
        ^^ { case n~v => Instrument(n, v) }
    | id ~ (LPAREN ~> repsep(id, COMMA) <~ RPAREN) ~ (EQUALS ~> expr)
        ^^ { case n~p~v => Assignment(n, p, v) }
    | (id <~ EQUALS) ~ expr ^^ { case n~v => Assignment(n, Seq(), v) }
    )
  }

  /**
   * <expr> := { <component>* }
   *         | <ident>
   *         | <number>
   */
  def expr: Parser[Expr] = positioned {
    ( LBRACE ~> (component *) <~ RBRACE ^^ { case c => Chain(c) }
    // At present, we only support parameter passing for applications in components. In general,
    // expressions can only be evaluated as 0-argument functions. This may change in the future.
    | id                                ^^ { case i => Application(i, Seq()) }
    | numberLiteral
    )
  }

  /**
   * <component> := <ident>(<exprs>)
   *              | <ident>
   */
  def component: Parser[Component] = positioned {
    ( id ~ (LPAREN ~> repsep(expr, COMMA) <~ RPAREN)
        ^^ { case i~a => AppComponent(Application(i, a)) }
    | id
        ^^ { case i => AppComponent(Application(i, Seq())) }
    )
  }

  def id: Parser[Ident] = positioned {
    accept("identifier", { case IDENT(name) => Ident(name) })
  }

  def numberLiteral: Parser[Num] = positioned {
    accept("number literal", { case NUMBER(n) => Num(n) })
  }

}

object CsppParser {

  def apply(tokens: Seq[CsppToken]) = {
    var csppParser = new CsppParser()
    csppParser.run(tokens)
  }

}
