package cspp

import scala.collection.mutable.HashSet
import scala.language.postfixOps
import scala.util.parsing.combinator._
import scala.util.parsing.input.{Reader,Position,NoPosition}

import absyn._
import tokens._

class CsppTokenReader(tokens: Seq[CsppToken], lastPos: Location = NoLocation) extends Reader[CsppToken] {
  override def first: CsppToken = tokens.head
  override def atEnd: Boolean = tokens.isEmpty
  override def pos: Location = tokens.headOption.map(_.loc).getOrElse(lastPos)
  override def rest: Reader[CsppToken] =
    // Packrat parsers look at the token-after-the-end's position, so we always keep track of the
    // next position after pos. That way if rest happens to be empty, we'll still have something
    // sensible to report to the parser.
    new CsppTokenReader(tokens.tail, Location(pos.line, pos.column + 1, pos.file))
}

class CsppParser(val disablingContext: CsppParser.DisablingContext = CsppParser.disablingContext())
  extends Parsers with PackratParsers
{

  override type Elem = CsppToken

  def disable(path: String) = require(disablingContext.add(path))

  def isDisabled(path: String) = disablingContext contains path

  def reader(tokens: Seq[CsppToken]) = new PackratReader(new CsppTokenReader(tokens))

  def apply(tokens: Seq[CsppToken]): Either[CsppParserError, Seq[Statement]] = {
    phrase(program)(reader(tokens)) match {
      case NoSuccess(msg, next: Input) =>
        Left(new CsppParserError(next.pos.asInstanceOf[Location], msg))
      case Success(result, _) => Right(result)
    }
  }

  def located[T <: CsppPositional](p: =>Parser[T]): Parser[T] = Parser { in =>
    p(in) match {
      case Success(t, in1) => Success((t setPos in.pos) inFile in.pos.asInstanceOf[Location].file, in1)
      case ns: NoSuccess => ns
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
  def statement: PackratParser[Statement] = located {
    ( (INSTR() ~> LPAREN() ~> rep1sep(expr, COMMA()) <~ RPAREN()) ~ (EQUALS() ~> expr)
        ^^ { case n~v => Instrument(n, v) }
    | id ~ (LPAREN() ~> repsep(id, COMMA()) <~ RPAREN()) ~ (EQUALS() ~> expr)
        ^^ { case n~p~v => Assignment(n, p, v) }
    | (id <~ EQUALS()) ~ expr ^^ { case n~v => Assignment(n, Seq(), v) }
    )
  }

  /**
   * <expr> := mux { <expr>* } <expr>
   *         | { <expr>* }
   *         | <expr> + <term>
   *         | <expr> - <term>
   *         | <term>
   */
  lazy val expr: PackratParser[Expr] = located {
    ( MUX() ~> (LBRACE() ~> (expr *) <~ RBRACE()) ~ expr ^^ { case es~m => Multiplexer(es, m) }
    | LBRACE() ~> (expr *) <~ RBRACE() ^^ { case c => Chain(c) }
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
  lazy val term: PackratParser[Expr] = located {
    ( term ~ STAR() ~ factor ^^ { case t~STAR()~f => BinOp(t, Times, f) }
    | term ~ SLASH() ~ factor ^^ { case t~SLASH()~f => BinOp(t, Divide, f) }
    | factor
    )
  }

  /**
   * <factor> := ( <expr> )
   *           | <application>
   *           | <number>
   */
  lazy val factor: PackratParser[Expr] = located {
    ( LPAREN() ~> expr <~ RPAREN()
    | application
    | numberLiteral
    )
  }

  def application: Parser[Application] = located {
    ( id ~ (LPAREN() ~> repsep(expr, COMMA()) <~ RPAREN()) ^^ { case i~a => Application(i ,a) }
    | id                                                   ^^ { case i => Application(i, Seq()) }
    )
  }

  def id: Parser[Ident] = located {
    accept("identifier", { case IDENT(name) =>  Ident(name) })
  }

  def file: Parser[String] = accept("file path", { case FILE(f) => f })

  def numberLiteral: Parser[Num] = located {
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
