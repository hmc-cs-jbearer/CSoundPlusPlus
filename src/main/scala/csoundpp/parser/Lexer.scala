package cspp

import scala.language.postfixOps
import scala.util.parsing.combinator._
import scala.util.parsing.input.Positional
import org.apache.commons.lang.StringEscapeUtils

trait CsppToken extends CsppPositional

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

class CsppLexer(val source: CsppFile) extends JavaTokenParsers with RegexParsers {

  def run: Either[CsppLexerError, Seq[CsppToken]] = parse(tokens, source.contents) match {
    case NoSuccess(msg, next) =>
      Left(new CsppLexerError(Location(next.pos.line, next.pos.column, source.path), msg))
    case Success(result, _)   => Right(result)
  }

  def processStringLiteral(literal: String) = {
    require(literal(0) == '"' && literal(literal.length - 1) == '"')
    val escaped = StringEscapeUtils.unescapeJava(literal)
    escaped.slice(1, escaped.length - 1)
  }

  def located[T <: CsppPositional](p: =>Parser[T]): Parser[T] = positioned {
    p ^^ { result => result inFile source.path }
  }

  // We can't skip newlines, because they affect how comments are parsed
  override def skipWhitespace = true
  override val whiteSpace = """[ \t\r\f]+""".r

  val id: Parser[CsppToken] = located {
    """[a-zA-Z][a-zA-Z0-9_]*""".r ^^ { s => IDENT(s) }
  }

  val num: Parser[CsppToken] = located {
    floatingPointNumber <~ not("""[a-zA-Z_]""".r) ^^ { n => NUMBER(n.toDouble) }
  }

  val file: Parser[CsppToken] = located {
    stringLiteral ^^ { s => FILE(processStringLiteral(s)) }
  }

  val importStmt: Parser[CsppToken] = located {
    "import" ^^^ IMPORT()
  }

  val instr: Parser[CsppToken] = located {
    "instr" ^^^ INSTR()
  }

  val lparen: Parser[CsppToken] = located {
    "(" ^^^ LPAREN()
  }

  val rparen: Parser[CsppToken] = located {
    ")" ^^^ RPAREN()
  }

  val lbrace: Parser[CsppToken] = located {
    "{" ^^^ LBRACE()
  }

  val rbrace: Parser[CsppToken] = located {
    "}" ^^^ RBRACE()
  }

  val comma: Parser[CsppToken] = located {
    "," ^^^ COMMA()
  }

  val equals: Parser[CsppToken] = located {
    "=" ^^^ EQUALS()
  }

  val star: Parser[CsppToken] = located {
    "*" ^^^ STAR()
  }

  val slash: Parser[CsppToken] = located {
    "/" ^^^ SLASH()
  }

  val plus: Parser[CsppToken] = located {
    "+" ^^^ PLUS()
  }

  val minus: Parser[CsppToken] = located {
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

object CsppLexer {
  def apply(source: CsppFile): Either[CsppCompileError, Seq[CsppToken]] = {
    new CsppLexer(source).run
  }

  def apply(source: String): Either[CsppCompileError, Seq[CsppToken]] = {
    apply(CsppFile("", source))
  }
}
