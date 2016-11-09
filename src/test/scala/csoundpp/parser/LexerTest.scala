package cspp

import org.scalatest.FunSuite
import org.scalatest.Matchers

class LexerSuite extends FunSuite with Matchers {

  def testGoodInput(input: String, output: Seq[CsppToken]) = {
    CsppLexer(input) should equal (Right(output))
  }

  def testBadInput(input: String, loc: Location) = CsppLexer(input) match {
    case Right(output)                          => fail(output.toString ++ " was successful")
    case Left(CsppCompileError(reportedLoc, _)) => reportedLoc should equal (loc)
  }

  implicit class LexerTester(input: String) {
    def ->(output: Seq[CsppToken]) = testGoodInput(input, output)
    def ->(output: CsppToken) = testGoodInput(input, Seq(output))
    def ->(error: LexError) = testBadInput(input, error)
  }

  // Object used to state expectation for tests that should fail the lexing phase
  class LexError(line: Int, column: Int) extends Location(line, column)

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Identifier tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  def testGoodIdent(input: String) = input -> IDENT(input)
  def testBadIdent(input: String, loc: Int) = input -> new LexError(1, loc)

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

  test("identifier.invalid.dash") {
    testBadIdent("foo-bar", 4)
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Number tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  test("number.integer") {
    "42" -> NUMBER(42)
  }

  test("number.decimal") {
    "3.14" -> NUMBER(3.14)
  }

  test("number.scientific") {
    "1e5" -> NUMBER(1e5)
  }

  test("number.list") {
    "1, 2,3" -> Seq(NUMBER(1), COMMA, NUMBER(2), COMMA, NUMBER(3))
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Comment tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  test("comments.inline.onlyComment") {
    "// This is a comment" -> Seq()
  }

  test("comments.inline.endOfLine") {
    "42 // This is a comment" -> NUMBER(42)
  }

  test("comments.inline.beforeLine") {
    "// This is a comment\n42" -> NUMBER(42)
  }

  test("comments.multiline") {
    "42\n42/* This\nis\na\ncomment\n*/42\n42" -> Seq(NUMBER(42), NUMBER(42), NUMBER(42), NUMBER(42))
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Simple token tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  test("tokens.instr") {
    "instr" -> INSTR
  }

  test("tokens.lparen") {
    "(" -> LPAREN
  }

  test("tokens.rparen") {
    ")" -> RPAREN
  }

  test("tokens.lbrace") {
    "{" -> LBRACE
  }

  test("tokens.rbrace") {
    "}" -> RBRACE
  }

  test("tokens.comma") {
    "," -> COMMA
  }

  test("tokens.equals") {
    "=" -> EQUALS
  }

  test("tokens.multiline") {
    "foo\n42\n(\n)\n//comment\n=" -> Seq(
      IDENT("foo"),
      NUMBER(42),
      LPAREN,
      RPAREN,
      EQUALS
    )
  }

}
