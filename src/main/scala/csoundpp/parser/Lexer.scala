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

class CsppLexer extends JavaTokenParsers with RegexParsers {

  // We can't skip newlines, because they affect how comments are parsed
  override def skipWhitespace = true
  override val whiteSpace = """[ \t\r\f]+""".r

  def run(source: String): Either[CsppLexerError, Seq[CsppToken]] =
  {
    parseAll(tokens, source) match {
      case NoSuccess(msg, next) =>
        Left(new CsppLexerError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, _)   => Right(result)
    }
  }

  def id: Parser[CsppToken] = positioned {
    """[a-zA-Z][a-zA-Z0-9_]*""".r ^^ { s => IDENT(s) }
  }

  def num: Parser[CsppToken] = positioned {
    floatingPointNumber <~ not("""[a-zA-Z_]""".r) ^^ { n => NUMBER(n.toDouble) }
  }

  def instr: Parser[CsppToken] = positioned {
    "instr" ^^ { _ => INSTR }
  }

  def lparen: Parser[CsppToken] = positioned {
    "(" ^^ { _ => LPAREN }
  }

  def rparen: Parser[CsppToken] = positioned {
    ")" ^^ { _ => RPAREN }
  }

  def lbrace: Parser[CsppToken] = positioned {
    "{" ^^ { _ => LBRACE }
  }

  def rbrace: Parser[CsppToken] = positioned {
    "}" ^^ { _ => RBRACE }
  }

  def comma: Parser[CsppToken] = positioned {
    "," ^^ { _ => COMMA }
  }

  def equals: Parser[CsppToken] = positioned {
    "=" ^^ { _ => EQUALS }
  }

  def ignore: Parser[Unit] =
    ( """//.*\n?""".r    ^^ { _ => () }
    | """/\*[^(\*)/]*\*/""".r  ^^ { _ => () }
    | "\n"              ^^ { _ => () }
    )

  def tokens: Parser[Seq[CsppToken]] = phrase((ignore *) ~>
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

object CsppLexer {
  def apply(source: String): Either[CsppLexerError, Seq[CsppToken]] =
  {
    var csppLexer = new CsppLexer()
    csppLexer.run(source)
  }
}
