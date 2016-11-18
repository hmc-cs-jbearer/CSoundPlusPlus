package cspp

import scala.collection.mutable.HashSet
import scala.language.postfixOps
import scala.util.parsing.combinator._
import scala.util.parsing.input.{Reader,Position,NoPosition}

// A simple Position class which we will use to keep track of the end token
case class Pos(line: Int, column: Int) extends Position {
  def lineContents = ""
}

class CsppTokenReader(tokens: Seq[CsppToken], lastPos: Pos = Pos(0, 0)) extends Reader[CsppToken] {
  override def first: CsppToken = tokens.head
  override def atEnd: Boolean = tokens.isEmpty
  override def pos: Position = tokens.headOption.map(_.pos).getOrElse(lastPos)
  override def rest: Reader[CsppToken] =
    // Packrat parsers look at the token-after-the-end's position, so we always keep track of the
    // next position after pos. That way if rest happens to be empty, we'll still have something
    // sensible to report to the parser.
    new CsppTokenReader(tokens.tail, Pos(pos.line, pos.column + 1))
}

class CsppParser(val disablingContext: CsppParser.DisablingContext = CsppParser.disablingContext())
  extends Parsers with PackratParsers
{

  override type Elem = CsppToken

  def disable(path: String) = require(disablingContext.add(path))

  def isDisabled(path: String) = disablingContext contains path

  def apply(tokens: Seq[CsppToken]): Either[CsppParserError, Seq[Statement]] = {
    phrase(program)(new CsppTokenReader(tokens)) match {
      case NoSuccess(msg, next) =>
        Left(new CsppParserError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, _) => Right(result)
    }
  }

  /**
   * <program> := <import>* <statement>*
   */
  def program: PackratParser[Seq[Statement]] =
    (importStmt *) ~ (statement *) ^^ { case i~s => i.flatten ++ s }

  def importStmt: PackratParser[Seq[Statement]] = IMPORT() ~> (file into sourceFile _)

  def sourceFile(file: String): Parser[Seq[Statement]] =
    if (isDisabled(file)) {
      success(Seq())
    } else {
      disable(file)

      val ast: Either[CsppCompileError, Seq[Statement]] = for {
        source <- CsppFileReader(file).right
        tokens <- CsppLexer(source).right

        /**
         * We pass in a reference to our disablingContext. This does two things:
         * 1. It forces the new parser to ignore any files we've already disabled.
         * 2. It allows the new parser to disable additional files, and we'll see the changes to
         * the disabling context when it finishes.
         */
        ast <- CsppParser(tokens, disablingContext).right
      } yield ast

      ast match {
        case Right(result) => success(result)
        case Left(CsppCompileError(pos, msg)) => err(s"Error in file $file ($pos): $msg")
      }
    }

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
  def statement: PackratParser[Statement] = positioned {
    ( (INSTR() ~> LPAREN() ~> rep1sep(expr, COMMA()) <~ RPAREN()) ~ (EQUALS() ~> expr)
        ^^ { case n~v => Instrument(n, v) }
    | id ~ (LPAREN() ~> repsep(id, COMMA()) <~ RPAREN()) ~ (EQUALS() ~> expr)
        ^^ { case n~p~v => Assignment(n, p, v) }
    | (id <~ EQUALS()) ~ expr ^^ { case n~v => Assignment(n, Seq(), v) }
    )
  }

  /**
   * <expr> := { <expr>* }
   *         | <expr> + <term>
   *         | <expr> - <term>
   *         | <term>
   */
  lazy val expr: PackratParser[Expr] = positioned {
    ( LBRACE() ~> (expr *) <~ RBRACE() ^^ { case c => Chain(c) }
    | expr ~ PLUS() ~ term ^^ { case e~PLUS()~t => BinOp(e, Plus, t) }
    | expr ~ MINUS() ~ term ^^ { case e~MINUS()~t => BinOp(e, Minus, t) }
    | term
    )
  }

  /**
   * <term> := <term> * <factor>
   *         | <term> / <factor>
   *         | <factor>
   */
  lazy val term: PackratParser[Expr] = positioned {
    ( term ~ STAR() ~ factor ^^ { case t~STAR()~f => BinOp(t, Times, f) }
    | term ~ SLASH() ~ factor ^^ { case t~SLASH()~f => BinOp(t, Divide, f) }
    | factor
    )
  }

  /**
   * <factor> := ( <expr> )
   *           | <ident> ( <exprs> )
   *           | <ident>
   *           | <number>
   */
  lazy val factor: PackratParser[Expr] = positioned {
    ( LPAREN() ~> expr <~ RPAREN()
    | id ~ (LPAREN() ~> repsep(expr, COMMA()) <~ RPAREN()) ^^ { case i~a => Application(i ,a) }
    | id ^^ { i => Application(i, Seq()) }
    | numberLiteral
    )
  }

  def id: Parser[Ident] = positioned {
    accept("identifier", { case IDENT(name) =>  Ident(name) })
  }

  def file: Parser[String] = accept("file path", { case FILE(f) => f })

  def numberLiteral: Parser[Num] = positioned {
    accept("number literal", { case NUMBER(n) =>  Num(n) })
  }

}

object CsppParser {

  // Used to keep track of which files have already been imported so we don't import anything twice.
  type DisablingContext = HashSet[String]

  def disablingContext(paths: String*): DisablingContext = {
    val context = new DisablingContext()
    for (path <- paths) {
      context.add(path)
    }
    context
  }

  def apply(tokens: Seq[CsppToken], disabled: DisablingContext = disablingContext()) = {
    val parser = new CsppParser(disabled)
    parser(tokens)
  }

}
