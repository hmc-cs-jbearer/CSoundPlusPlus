package cspp

import scala.language.postfixOps
import scala.util.parsing.combinator._

case class CsppParserError(location: Location, msg: String)

case class Location(line: Int, column: Int) {
  override def toString = s"$line:$column"
}

object CsppParser extends JavaTokenParsers with RegexParsers {

  def apply(source: String): Either[CsppParserError, Seq[Statement]] =
  parseAll(program, source) match {
    case NoSuccess(msg, next) =>
      Left(CsppParserError(Location(next.pos.line, next.pos.column), msg))
    case Success(result, _) => Right(result)
  }

  /**
   * <program> := <statement> <program>
   *            | <empty>
   */
  lazy val program: Parser[Seq[Statement]] = statement *

  /**
   * <statement> := instr( <exprs> ) = <expr>
   *              | <ident> = <expr>
   *
   * <exprs> := <expr>
   *          | <expr>, <exprs>
   */
  lazy val statement: Parser[Statement] = positioned {
    ( ("instr" ~> "(" ~> rep1sep(expr, ",") <~ ")") ~ ("=" ~> expr)
        ^^ { case n~v => Instrument(n, v) }
    | (id <~ "=") ~ expr ^^ { case n~v => Assignment(n, v) }
    )
  }

  /**
   * <expr> := { <component>* }
   *         | <ident>
   *         | <number>
   */
  lazy val expr: Parser[Expr] = positioned {
    ( "{" ~> (component *) <~ "}" ^^ { case c => Chain(c) }
    | id                          ^^ { case i => Var(i) }
    | floatingPointNumber         ^^ { case n => Num(n.toDouble) }
    )
  }

  /**
   * <component> := <ident>(<exprs>)
   *              | <ident>
   */
  lazy val component: Parser[Component] = positioned {
    ( id ~ ("(" ~> repsep(expr, ",") <~ ")") ^^ { case i~a => VarComponent(i, a) }
    | id                                     ^^ { case i => VarComponent(i, Seq()) }
    )
  }

  /**
   * Identifiers must:
   *   * start with a letter
   *   * contain only letters, numbers, and underscores
   *   * not be the same as a keyword
   */
  lazy val id: Parser[Ident] = positioned {
    not(keyword) ~> ("""[a-zA-Z][a-zA-Z0-9_]*""".r) ^^ { case i => Ident(i) }
  }

  // Keywords to be excluded from the set of valid identifiers
  lazy val keyword: Parser[String] = "instr"

}
