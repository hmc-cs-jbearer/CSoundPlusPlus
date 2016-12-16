package cspp.csound

import scala.language.implicitConversions
import scala.util.parsing.combinator._
import scala.util.parsing.input.Positional

import cspp._

abstract class CsToken extends Positional {
  val tok: String
  override def toString = tok
}

// Tokens like identifiers, which need spaces around them. Specifically, any two consecutive space
// tokens will get a space between them. All other whitespace will be eliminated.
case class SpaceToken(tok: String) extends CsToken

// Tokens like commas, which do not need spaces
case class NoSpaceToken(tok: String) extends CsToken

// Tokens like comments, which should be ignored
object IgnoreToken extends CsToken {
  val tok = ""
}

object Space extends NoSpaceToken(" ")


case class CsTokenString(toks: Seq[CsToken]) {
  def +(tok: CsToken): CsTokenString =
    if (toks.isEmpty) {
      CsTokenString(Seq(tok))
    } else {
      (toks.last, tok) match {
        case (_, IgnoreToken) => this

        // If both tokens need a space, we have to add one in
        case (SpaceToken(_), SpaceToken(_)) => CsTokenString(toks :+ Space :+ tok)

        // If one of the tokens is a no-space token, we don't need a space
        case _ => CsTokenString(toks :+ tok)
      }
    }

  def mkString(sep: String = ""): String = toks.mkString(sep)
  def mkString: String = mkString()
}

object CsTokenString {
  implicit def Token2TokenString(tok: CsToken) = CsTokenString(Seq(tok))
}

trait CsParsers extends Parsers {
  /**
   * Create a parser which uses the parser p to build up a token string by apply p zero or more
   * times until it fails.
   */
  def tokenStringRep(p: Parser[CsToken], toks: CsTokenString = CsTokenString(Seq())): Parser[CsTokenString] =
    Parser { in =>
      p(in) match {
        case ns: NoSuccess => Success(toks, in)
        case Success(tok, rest) => tokenStringRep(p, toks + tok)(rest)
      }
    }
}

object CsoundMinifier extends JavaTokenParsers with RegexParsers with CsParsers {

  type CsLine = CsppTranslator.CsLine
  type CsLines = CsppTranslator.CsLines

  def apply(input: CsLines): Either[CsParseError, CsLines] = {

    def continue(input: CsLines, f: CsLines => CsLines): Either[CsParseError, CsLines] =
      apply(input) match {
        case Right(output) => Right(f(output))
        case Left(err) => Left(err)
      }

    if (input.isEmpty) {
      Right(Seq())
    } else {
      apply(input.head) match {

        // Ignore empty lines
        case Right("") => continue(input.tail, (tail: CsLines) => tail)

        case Right(minifiedHead) => continue(input.tail, minifiedHead +: _)

        case Left(err) => Left(err)
      }
    }
  }

  def apply(input: CsLine): Either[CsParseError, CsLine] =
    parse(tokens, input) match {
      case Success(toks, _) => Right(toks.mkString)
      case NoSuccess(msg, next) => Left(new CsParseError(next.pos, msg))
    }

  def tokens: Parser[CsTokenString] = phrase(tokenStringRep(token))

  def token: Parser[CsToken] = spaceToken | noSpaceToken | ignoreToken

  def spaceToken: Parser[CsToken] =
    ( ident   // Any keywords, ids, etc will match
    | "0dbfs" // This one fails to match ident but confuses floatingPointNumber, so we explicitly match it
    | floatingPointNumber
    ) ^^ { SpaceToken(_) }

  def noSpaceToken: Parser[CsToken] =
    ( stringLiteral
    | "#" // Macro definition
    | "$" // Macro expansion
    | "'" // Macro argument separator
    | "."
    | "+" | "-" | "*" | "/" | "^" | "=" | "<" | ">"
    | "(" | ")"
    | ","
    | "!"
    ) ^^ { NoSpaceToken(_) }

  def ignoreToken: Parser[CsToken] = phrase(";.*".r) ^^^ IgnoreToken

}
