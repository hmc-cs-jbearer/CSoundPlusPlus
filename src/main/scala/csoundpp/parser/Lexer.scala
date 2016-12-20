package cspp

import scala.language.postfixOps
import scala.util.parsing.combinator._
import org.apache.commons.lang.StringEscapeUtils

import tokens._

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

  def word(s: String): Parser[String] = s <~ not("""[a-zA-Z0-9_]""".r)

  override def skipWhitespace = false

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
    word("import") ^^^ IMPORT()
  }

  val instr: Parser[CsppToken] = located {
    word("instr") ^^^ INSTR()
  }

  val inserts: Parser[CsppToken] = located {
    word("inserts") ^^^ INSERTS()
  }

  val sends: Parser[CsppToken] = located {
    word("sends") ^^^ SENDS()
  }

  val parallel: Parser[CsppToken] = located {
    word("parallel") ^^^ PARALLEL()
  }

  val let: Parser[CsppToken] = located {
    word("let") ^^^ LET()
  }

  val in: Parser[CsppToken] = located {
    word("in") ^^^ IN()
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
    | whiteSpace ^^ { _ => () }
    )

  val tokens: Parser[Seq[CsppToken]] = phrase((ignore *) ~>
    rep(
      // Keywords first
      ( importStmt
      | instr
      | inserts
      | sends
      | parallel
      | let
      | in

      // Then special characters
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

      // Literals
      | file
      | num

      // Identifiers
      | id
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
