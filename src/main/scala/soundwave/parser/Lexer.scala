package soundwave

import scala.language.postfixOps
import scala.util.parsing.combinator._
import org.apache.commons.lang.StringEscapeUtils

import tokens._

class SwLexer(val source: SwFile) extends JavaTokenParsers with RegexParsers {

  def run: Either[SwLexerError, Seq[SwToken]] = parse(tokens, source.contents) match {
    case NoSuccess(msg, next) =>
      Left(new SwLexerError(Location(next.pos.line, next.pos.column, source.path), msg))
    case Success(result, _)   => Right(result)
  }

  def processStringLiteral(literal: String) = {
    require(literal(0) == '"' && literal(literal.length - 1) == '"')
    val escaped = StringEscapeUtils.unescapeJava(literal)
    escaped.slice(1, escaped.length - 1)
  }

  def located[T <: SwPositional](p: =>Parser[T]): Parser[T] = positioned {
    p ^^ { result => result inFile source.path }
  }

  def word(s: String): Parser[String] = s <~ not("""[a-zA-Z0-9_]""".r)

  override def skipWhitespace = false

  val id: Parser[SwToken] = located {
    """[a-zA-Z][a-zA-Z0-9_]*""".r ^^ { s => IDENT(s) }
  }

  val num: Parser[SwToken] = located {
    floatingPointNumber <~ not("""[a-zA-Z_]""".r) ^^ { n => NUMBER(n.toDouble) }
  }

  val file: Parser[SwToken] = located {
    stringLiteral ^^ { s => FILE(processStringLiteral(s)) }
  }

  val importStmt: Parser[SwToken] = located {
    word("import") ^^^ IMPORT()
  }

  val instr: Parser[SwToken] = located {
    word("instr") ^^^ INSTR()
  }

  val inserts: Parser[SwToken] = located {
    word("inserts") ^^^ INSERTS()
  }

  val sends: Parser[SwToken] = located {
    word("sends") ^^^ SENDS()
  }

  val parallel: Parser[SwToken] = located {
    word("parallel") ^^^ PARALLEL()
  }

  val let: Parser[SwToken] = located {
    word("let") ^^^ LET()
  }

  val in: Parser[SwToken] = located {
    word("in") ^^^ IN()
  }

  val lparen: Parser[SwToken] = located {
    "(" ^^^ LPAREN()
  }

  val rparen: Parser[SwToken] = located {
    ")" ^^^ RPAREN()
  }

  val lbrace: Parser[SwToken] = located {
    "{" ^^^ LBRACE()
  }

  val rbrace: Parser[SwToken] = located {
    "}" ^^^ RBRACE()
  }

  val comma: Parser[SwToken] = located {
    "," ^^^ COMMA()
  }

  val equals: Parser[SwToken] = located {
    "=" ^^^ EQUALS()
  }

  val star: Parser[SwToken] = located {
    "*" ^^^ STAR()
  }

  val slash: Parser[SwToken] = located {
    "/" ^^^ SLASH()
  }

  val plus: Parser[SwToken] = located {
    "+" ^^^ PLUS()
  }

  val minus: Parser[SwToken] = located {
    "-" ^^^ MINUS()
  }

  val ignore: Parser[Unit] =
    ( """//.*(\n|$)""".r    ^^ { _ => () }
    | """/\*([^\*]|\*[^/])*\*/""".r  ^^ { _ => () }
    | whiteSpace ^^ { _ => () }
    )

  val tokens: Parser[Seq[SwToken]] = phrase((ignore *) ~>
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

object SwLexer {
  def apply(source: SwFile): Either[SwCompileError, Seq[SwToken]] = {
    new SwLexer(source).run
  }

  def apply(source: String): Either[SwCompileError, Seq[SwToken]] = {
    apply(SwFile("", source))
  }
}
