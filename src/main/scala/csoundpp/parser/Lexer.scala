package cspp

import scala.language.postfixOps
import scala.util.parsing.combinator._
import scala.util.parsing.input.Positional

trait CsppToken extends Positional

case class IDENT(str: String) extends CsppToken
case class NUMBER(num: Double) extends CsppToken
case object INSTR extends CsppToken
case object LPAREN extends CsppToken
case object RPAREN extends CsppToken
case object LBRACE extends CsppToken
case object RBRACE extends CsppToken
case object COMMA extends CsppToken
case object EQUALS extends CsppToken

object CsppLexer extends JavaTokenParsers with RegexParsers {

  def apply(source: String): Either[CsppLexerError, Seq[CsppToken]] = parse(tokens, source) match {
    case NoSuccess(msg, next) => Left(CsppLexerError(Location(next.pos.line, next.pos.column), msg))
    case Success(result, _)   => Right(result)
  }

  // We can't skip newlines, because they affect how comments are parsed
  override def skipWhitespace = true
  override val whiteSpace = """[ \t\r\f]+""".r

  val id: Parser[CsppToken] = positioned {
    """[a-zA-Z][a-zA-Z0-9_]*""".r ^^ { s => IDENT(s) }
  }

  val num: Parser[CsppToken] = positioned {
    floatingPointNumber <~ not("""[a-zA-Z_]""".r) ^^ { n => NUMBER(n.toDouble) }
  }

  val instr: Parser[CsppToken] = positioned {
    "instr" ^^ { _ => INSTR }
  }

  val lparen: Parser[CsppToken] = positioned {
    "(" ^^ { _ => LPAREN }
  }

  val rparen: Parser[CsppToken] = positioned {
    ")" ^^ { _ => RPAREN }
  }

  val lbrace: Parser[CsppToken] = positioned {
    "{" ^^ { _ => LBRACE }
  }

  val rbrace: Parser[CsppToken] = positioned {
    "}" ^^ { _ => RBRACE }
  }

  val comma: Parser[CsppToken] = positioned {
    "," ^^ { _ => COMMA }
  }

  val equals: Parser[CsppToken] = positioned {
    "=" ^^ { _ => EQUALS }
  }

  val ignore: Parser[Unit] =
    ( """//.*\n?""".r    ^^ { _ => () }
    | """/\*[^(\*)/]*\*/""".r  ^^ { _ => () }
    | "\n"              ^^ { _ => () }
    )

  val tokens: Parser[Seq[CsppToken]] = phrase((ignore *) ~>
    rep(
      ( instr
      | lparen
      | rparen
      | lbrace
      | rbrace
      | comma
      | equals
      | id
      | num
      ) <~ rep(ignore)
    ))
}
