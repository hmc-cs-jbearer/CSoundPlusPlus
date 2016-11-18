package cspp

import scala.language.postfixOps
import scala.util.parsing.combinator._
import scala.util.parsing.input.Positional
import org.apache.commons.lang.StringEscapeUtils

trait CsppToken extends Positional

case class IDENT(str: String) extends CsppToken
case class NUMBER(num: Double) extends CsppToken
case class FILE(str: String) extends CsppToken

// Sadly, these must all be case classes (not objects) because they have a mutable position field
case class IMPORT() extends CsppToken
case class INSTR() extends CsppToken
case class LPAREN() extends CsppToken
case class RPAREN() extends CsppToken
case class LBRACE() extends CsppToken
case class RBRACE() extends CsppToken
case class COMMA() extends CsppToken
case class EQUALS() extends CsppToken
case class STAR() extends CsppToken
case class SLASH() extends CsppToken
case class PLUS() extends CsppToken
case class MINUS() extends CsppToken

object CsppLexer extends JavaTokenParsers with RegexParsers {

  def apply(source: String): Either[CsppLexerError, Seq[CsppToken]] = parse(tokens, source) match {
    case NoSuccess(msg, next) =>
      Left(new CsppLexerError(Location(next.pos.line, next.pos.column), msg))
    case Success(result, _)   => Right(result)
  }

  def processStringLiteral(literal: String) = {
    require(literal(0) == '"' && literal(literal.length - 1) == '"')
    val escaped = StringEscapeUtils.unescapeJava(literal)
    escaped.slice(1, escaped.length - 1)
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

  val file: Parser[CsppToken] = positioned {
    stringLiteral ^^ { s => FILE(processStringLiteral(s)) }
  }

  val importStmt: Parser[CsppToken] = positioned {
    "import" ^^^ IMPORT()
  }

  val instr: Parser[CsppToken] = positioned {
    "instr" ^^^ INSTR()
  }

  val lparen: Parser[CsppToken] = positioned {
    "(" ^^^ LPAREN()
  }

  val rparen: Parser[CsppToken] = positioned {
    ")" ^^^ RPAREN()
  }

  val lbrace: Parser[CsppToken] = positioned {
    "{" ^^^ LBRACE()
  }

  val rbrace: Parser[CsppToken] = positioned {
    "}" ^^^ RBRACE()
  }

  val comma: Parser[CsppToken] = positioned {
    "," ^^^ COMMA()
  }

  val equals: Parser[CsppToken] = positioned {
    "=" ^^^ EQUALS()
  }

  val star: Parser[CsppToken] = positioned {
    "*" ^^^ STAR()
  }

  val slash: Parser[CsppToken] = positioned {
    "/" ^^^ SLASH()
  }

  val plus: Parser[CsppToken] = positioned {
    "+" ^^^ PLUS()
  }

  val minus: Parser[CsppToken] = positioned {
    "-" ^^^ MINUS()
  }

  val ignore: Parser[Unit] =
    ( """//.*(\n|$)""".r    ^^ { _ => () }
    | """/\*([^\*]|\*[^/])*\*/""".r  ^^ { _ => () }
    | "\n"              ^^ { _ => () }
    )

  val tokens: Parser[Seq[CsppToken]] = phrase((ignore *) ~>
    rep(
      ( importStmt
      | instr
      | lparen
      | rparen
      | lbrace
      | rbrace
      | comma
      | equals
      | star
      | slash
      | plus
      | minus
      | file
      | id
      | num
      ) <~ rep(ignore)
    ))
}
