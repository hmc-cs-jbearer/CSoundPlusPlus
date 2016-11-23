package cspp

import org.scalatest.FunSuite
import org.scalatest.Matchers

import tokens._

class LexerSuite extends FunSuite with Matchers {

  def testGoodInputWithLocation(input: String, output: Seq[(CsppToken, Int, Int)]) = {
    CsppLexer(input) match {
      case Left(err) => fail(err)
      case Right(tokens) => {
        tokens.length should equal (output.length)
        for ((tok, (expectedTok, line, col)) <- tokens zip output) {
          tok should equal (expectedTok)
          (tok.pos.line, tok.pos.column) should equal ((line, col))
        }
      }
    }
  }

  def testGoodInput(input: String, output: Seq[CsppToken]) = {
    CsppLexer(input) should equal (Right(output))
  }

  def testBadInput(input: String, error: LexError) = CsppLexer(input) match {
    case Right(output)                          => fail(output.toString ++ " was successful")
    case Left(CsppCompileError(reportedLoc, _)) => {
      reportedLoc.line should equal (error.line)
      reportedLoc.column should equal (error.column)
    }
  }

  implicit class LexerTester(input: String) {
    def ~>(output: Seq[(CsppToken, Int, Int)]) = testGoodInputWithLocation(input, output)
    def ~>(output: Seq[CsppToken]) = testGoodInput(input, output)
    def ~>(output: CsppToken) = testGoodInput(input, Seq(output))
    def ~>(error: LexError) = testBadInput(input, error)
  }

  // Object used to state expectation for tests that should fail the lexing phase
  class LexError(line: Int, column: Int) extends Location(line, column, "")

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Identifier tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  def testGoodIdent(input: String) = input ~> IDENT(input)
  def testBadIdent(input: String, loc: Int) = input ~> new LexError(1, loc)

  test("identifier.lowercase") {
    testGoodIdent("foo")
  }

  test("identifier.camelcase") {
    testGoodIdent("fooBar")
  }

  test("identifier.uppercamel") {
    testGoodIdent("fooBar")
  }

  test("identifier.underscore") {
    testGoodIdent("foo_bar")
  }

  test("identifier.numbers") {
    testGoodIdent("foo1")
  }

  test("identifier.invalid.number") {
    testBadIdent("1foo", 3)
  }

  test("identifier.invalid.underscore") {
    testBadIdent("_foo", 1)
  }

  test("identifier.invalid.dot") {
    testBadIdent("foo.bar", 4)
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // File tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  test("file.normal") {
    "\"~/Documents/myFile.txt\"" ~> FILE("~/Documents/myFile.txt")
  }

  test("file.backslashes") {
    "\"C:\\\\Users\\\\jbearer\\\\Documents\\\\myFile.txt\"" ~>
      FILE("C:\\Users\\jbearer\\Documents\\myFile.txt")
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Number tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  test("number.integer") {
    "42" ~> NUMBER(42)
  }

  test("number.decimal") {
    "3.14" ~> NUMBER(3.14)
  }

  test("number.scientific") {
    "1e5" ~> NUMBER(1e5)
  }

  test("number.list") {
    "1, 2,3" ~> Seq(NUMBER(1), COMMA(), NUMBER(2), COMMA(), NUMBER(3))
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Import tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  test("import.keyword") {
    "import" ~> IMPORT()
  }

  test("import.withFile") {
    "import \"~/Documents/myFile.txt\"" ~> Seq(IMPORT(), FILE("~/Documents/myFile.txt"))
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Comment tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  test("comments.inline.onlyComment") {
    "// This is a comment" ~> Seq[CsppToken]()
  }

  test("comments.inline.endOfLine") {
    "42 // This is a comment" ~> NUMBER(42)
  }

  test("comments.inline.beforeLine") {
    "// This is a comment\n42" ~> NUMBER(42)
  }

  test("comments.multiline.beforeLine") {
    "/* This\nis\na\ncomment\n*/42\n42" ~> Seq(NUMBER(42), NUMBER(42))
  }

  test("comments.multiline.afterLine") {
    "42\n42/* This\nis\na\ncomment\n*/42\n42" ~> Seq(NUMBER(42), NUMBER(42), NUMBER(42), NUMBER(42))
  }

  test("comments.multiline.withStars") {
    "/* This\n * is\n * a comment\n */" ~> Seq[CsppToken]()
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Simple token tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  test("tokens.instr") {
    "instr" ~> INSTR()
  }

  test("tokens.lparen") {
    "(" ~> LPAREN()
  }

  test("tokens.rparen") {
    ")" ~> RPAREN()
  }

  test("tokens.lbrace") {
    "{" ~> LBRACE()
  }

  test("tokens.rbrace") {
    "}" ~> RBRACE()
  }

  test("tokens.comma") {
    "," ~> COMMA()
  }

  test("tokens.equals") {
    "=" ~> EQUALS()
  }

  test("tokens.mux") {
    "mux" -> MUX()
  }

  test("tokens.multiline") {
    "foo\n42\n(\n)\n//comment\n=" ~> Seq(
      IDENT("foo"),
      NUMBER(42),
      LPAREN(),
      RPAREN(),
      EQUALS()
    )
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Location tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  test("location") {
    """foo = 1
    |
    |source = {
    |  fm(foo, 440)
    |}
    |
    """.stripMargin ~> Seq(
      (IDENT("foo"), 1, 1),
      (EQUALS(), 1, 5),
      (NUMBER(1), 1, 7),
      (IDENT("source"), 3, 1),
      (EQUALS(), 3, 8),
      (LBRACE(), 3, 10),
      (IDENT("fm"), 4, 3),
      (LPAREN(), 4, 5),
      (IDENT("foo"), 4, 6),
      (COMMA(), 4, 9),
      (NUMBER(440), 4, 11),
      (RPAREN(), 4, 14),
      (RBRACE(), 5, 1)
    )
  }

}
