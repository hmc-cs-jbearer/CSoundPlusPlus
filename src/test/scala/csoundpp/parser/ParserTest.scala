package cspp

import org.scalatest.FunSuite
import org.scalatest.Matchers
import scala.util.parsing.input.CharSequenceReader

class ParserSuite extends FunSuite with Matchers {

  // Parse the input string using a specific parser.
  def parse[T](input: String, parser: CsppParser.Parser[T]) =
    CsppParser.phrase(parser)(new CharSequenceReader(input))

  // Assert that an input parses successfully and gives correct output.
  def testGoodInput[T](input: String, output: T, parser: CsppParser.Parser[T]) = {
    val result = parse(input, parser)
    result shouldBe 'successful
    result.get should equal (output)
  }

  // Assert that an input failes to parse.
  def testBadInput[T](input: String, parser:CsppParser.Parser[T]) = {
    parse(input, parser) should not be 'successful
  }

  /**
   * Test the parser beginning from the top level nonterminal (<program>).
   * Assert that a program parses successfully and gives the correct output.
   */
  def testGoodProgram(input: String, output: Seq[Statement]) = {
    CsppParser(input) should equal (Right(output))
  }

  /**
   * Test the parser beginning from the top level nonterminal (<program>).
   * Assert that a program fails to parse and that the error occurs at the specified line and
   * column.
   */
  def testBadProgram(input: String, loc: Location) = CsppParser(input) match {
    case Right(output)                          => fail(output.toString ++ " was successful")
    case Left(CsppParserError(reportedLoc, _))  => reportedLoc should equal (loc)
  }

  implicit class ParseTester(input: String) {
    def ->(output: Ident) = testGoodInput(input, output, CsppParser.id)
    def -/>(output: Ident) = testBadInput(input, CsppParser.id)

    def ->(output: Component) = testGoodInput(input, output, CsppParser.component)
    def -/>(output: Component) = testBadInput(input, CsppParser.component)

    def ->(output: Expr) = testGoodInput(input, output, CsppParser.expr)
    def -/>(output: Expr) = testBadInput(input, CsppParser.expr)

    def ->(output: Statement) = testGoodInput(input, output, CsppParser.statement)
    def -/>(output: Statement) = testBadInput(input, CsppParser.statement)

    def ->(output: Seq[Statement]) = testGoodProgram(input, output)
    def ->(error: ParseError) = testBadProgram(input, error)
  }

  // Overload selectors for -/> test
  object ident extends Ident("")
  object component extends Component
  object expr extends Expr
  object statement extends Statement

  // Object used to state expectation for tests that should fail to parse
  class ParseError(line: Int, column: Int) extends Location(line, column)

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Identifier tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  def testGoodIdent(input: String) = input -> Ident(input)
  def testBadIdent(input: String) = input -/> ident

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
    testBadIdent("1foo")
  }

  test("identifier.invalid.underscore") {
    testBadIdent("_foo")
  }

  test("identifier.invalid.dot") {
    testBadIdent("foo.bar")
  }

  test("identifier.invalid.dash") {
    testBadIdent("foo-bar")
  }

  test("identifier.invalid.space") {
    testBadIdent("foo bar")
  }

  test("identifier.invalid.keyword.instr") {
    testBadIdent("instr")
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Component tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  test("component.nullary") {
    "foo" -> VarComponent(Ident("foo"), Seq())
  }

  test("component.oneary") {
    "foo(42)" -> VarComponent(Ident("foo"), Seq(Num(42)))
  }

  test("component.polyary") {
    "foo(42, 43, 44)" -> VarComponent(Ident("foo"), Seq(Num(42), Num(43), Num(44)))
  }

  test("component.invalid.noCommas") {
    "foo(42 43 44)" -/> component
  }

  test("component.invalid.unmatchedLParen") {
    "foo(42" -/> component
  }

  test("component.invalid.unmatchedRParen") {
    "foo 42)" -/> component
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Expression tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  test("expression.num.integer") {
    "42" -> Num(42)
  }

  test("expression.num.decimal") {
    "3.14" -> Num(3.14)
  }

  test("expression.num.scientific") {
    "1e5" -> Num(1e5)
  }

  test("expression.var") {
    "foo" -> Var(Ident("foo"))
  }

  test("expression.chain.empty") {
    "{}" -> Chain(Seq())
  }

  test("expression.chain.one") {
    "{foo}" -> Chain(Seq(VarComponent(Ident("foo"), Seq())))
  }

  test("expressions.chain.many") {
    "{foo bar baz}" -> Chain(Seq(
      VarComponent(Ident("foo"), Seq()),
      VarComponent(Ident("bar"), Seq()),
      VarComponent(Ident("baz"), Seq())
    ))
  }

  test("expression.chain.oneWithArgs") {
    "{ foo(42, 43, 44) }" -> Chain(Seq(VarComponent(Ident("foo"), Seq(Num(42), Num(43), Num(44)))))
  }

  test("expression.chain.manyWithArgs") {
    "{ foo(42) bar(43) baz(42, 43) }" -> Chain(Seq(
      VarComponent(Ident("foo"), Seq(Num(42))),
      VarComponent(Ident("bar"), Seq(Num(43))),
      VarComponent(Ident("baz"), Seq(Num(42), Num(43)))
    ))
  }

  test("expressions.chain.mixedArgs") {
    "{ foo bar(41) }" -> Chain(Seq(
      VarComponent(Ident("foo"), Seq()),
      VarComponent(Ident("bar"), Seq(Num(41)))
    ))
  }

  test("expressions.chain.invalid.noBraces") {
    "foo bar baz" -/> expr
  }

  test("expressions.chain.invalid.unmatchedLBrace") {
    "{ foo bar baz" -/> expr
  }

  test("expressions.chain.invalid.unmatchedRBrace") {
    "foo bar baz }" -/> expr
  }

  test("expressions.chain.invalid.commas") {
    "{ foo, bar, baz }" -/> expr
  }

  test("expressions.chain.invalid.semicolons") {
    "{ foo; bar; baz }" -/> expr
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Statement tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  test("statement.assignment.ident") {
    "foo = bar" -> Assignment(Ident("foo"), Var(Ident("bar")))
  }

  test("statement.assignment.number") {
    "foo = 42" -> Assignment(Ident("foo"), Num(42))
  }

  test("statement.assignment.chain") {
    "foo = { bar }" -> Assignment(Ident("foo"), Chain(Seq(VarComponent(Ident("bar"), Seq()))))
  }

  test("statement.assignment.invalid.chain") {
    "foo = bar baz bat" -/> statement
  }

  test("statement.assignment.invalid.undefined") {
    "foo" -/> statement
  }

  test("statement.assignment.invalid.incomplete") {
    "foo =" -/> statement
  }

  test("statement.instrument.oneChannel") {
    "instr(1) = foo" -> Instrument(Seq(Num(1)), Var(Ident("foo")))
  }

  test("statement.instrument.manyChannels") {
    "instr(1, 3, channel) = { foo bar }" ->
      Instrument(Seq(Num(1), Num(3), Var(Ident("channel"))), Chain(Seq(
        VarComponent(Ident("foo"), Seq()),
        VarComponent(Ident("bar"), Seq())
      )))
  }

  test("statement.instrument.invalid.noChannels") {
    "instr() = foo" -/> statement
  }

  test("statement.instrument.invalid.noParens") {
    "instr = foo" -/> statement
  }

  test("statement.instrument.invalid.unmatchedLParen") {
    "instr(1 = foo" -/> statement
  }

  test("statement.instrument.invalid.unmatchedRParen") {
    "instr 1) = foo" -/> statement
  }

  test("statement.instrument.invalid.noKeyword") {
    "(1) = foo" -/> statement
  }

  test("statement.instrument.invalid.wrongKeyword") {
    "instrument(1) = foo" -/> statement
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Program tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  test("program.empty") {
    "" -> Seq()
  }

  test("program.valid") {
    """
    foo = 1
    bar = foo

    source = {
      fm(bar, 440, 10)
    }

    effect = {
      compress(40, bar)
    }

    instr(foo) = source

    instr(2, 3) = {
      source
      effect
    }
    """ ->
    Seq(
      Assignment(Ident("foo"), Num(1)),
      Assignment(Ident("bar"), Var(Ident("foo"))),
      Assignment(Ident("source"), Chain(Seq(
        VarComponent(Ident("fm"), Seq(Var(Ident("bar")), Num(440), Num(10)))
      ))),
      Assignment(Ident("effect"), Chain(Seq(
        VarComponent(Ident("compress"), Seq(Num(40), Var(Ident("bar"))))
      ))),
      Instrument(Seq(Var(Ident("foo"))), Var(Ident("source"))),
      Instrument(Seq(Num(2), Num(3)), Chain(Seq(
        VarComponent(Ident("source"), Seq()),
        VarComponent(Ident("effect"), Seq())
      )))
    )
  }

  test("program.invalid.firstLine") {
    """instr = 1
    foo = 1
    bar = foo

    source = {
      fm(bar, 440, 10)
    }

    effect = {
      compress(40, bar)
    }

    instr(foo) = source

    instr(2, 3) = {
      source
      effect
    }
    """ ->
    new ParseError(1, 7)
  }

  test("program.invalid.middle") {
    """
    |foo = 1
    |bar = foo
    |
    |source = {
    |  fm(bar, 440, 10)
    |}
    |
    |effect = {
    |  compress(40, bar)
    |}
    |
    |instr(foo) = _source
    |
    |instr(2, 3) = {
    |  source
    |  effect
    |}
    """.stripMargin ->
    new ParseError(13, 14)
  }

  test("program.invalid.lastLine") {
    """
    |foo = 1
    |bar = foo
    |
    |source = {
    |  fm(bar, 440, 10)
    |}
    |
    |effect = {
    |  compress(40, bar)
    |}
    |
    |instr(2, 3) = {
    |  source
    |  effect
    |}
    |
    |instr(4) = {
    |  source
    |  effect""".stripMargin ->
    new ParseError(20, 9)
  }
}
