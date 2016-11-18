package cspp

import org.scalatest.FunSuite
import org.scalatest.Matchers
import scala.util.parsing.input.CharSequenceReader

class ParserSuite extends FunSuite with Matchers {

  // Parse the input tokens using a specific parser.
  def parse[T](input: Seq[CsppToken], parser: CsppParser.Parser[T]) =
    CsppParser.phrase(parser)(new CsppTokenReader(input))

  // Assert that an input parses successfully and gives correct output.
  def testGoodInput[T](input: Seq[CsppToken], output: T, parser: CsppParser.Parser[T]) = {
    val result = parse(input, parser)
    result shouldBe 'successful
    result.get should equal (output)
  }

  // Assert that an input failes to parse.
  def testBadInput[T](input: Seq[CsppToken], parser:CsppParser.Parser[T]) = {
    parse(input, parser) should not be 'successful
  }

  /**
   * Test the parser beginning from the top level nonterminal (<program>).
   * Assert that a program parses successfully and gives the correct output.
   */
  def testGoodProgram(input: Seq[CsppToken], output: Seq[Statement]) = {
    CsppParser(input) should equal (Right(output))
  }

  /**
   * Test the parser beginning from the top level nonterminal (<program>).
   * Assert that a program fails to parse and that the error occurs at the specified line and
   * column.
   */
  def testBadProgram(input: Seq[CsppToken], loc: Location) = CsppParser(input) match {
    case Right(output)                          => fail(output.toString ++ " was successful")
    case Left(CsppCompileError(reportedLoc, _)) => reportedLoc should equal (loc)
  }

  implicit class ParseTester(input: Seq[CsppToken]) {
    def ~>(output: Ident) = testGoodInput(input, output, CsppParser.id)
    def ~/>(output: ident.type) = testBadInput(input, CsppParser.id)

    def ~>(output: Component) = testGoodInput(input, output, CsppParser.component)
    def ~/>(output: component.type) = testBadInput(input, CsppParser.component)

    def ~>(output: Expr) = testGoodInput(input, output, CsppParser.expr)
    def ~/>(output: expr.type) = testBadInput(input, CsppParser.expr)

    def ~>(output: Statement) = testGoodInput(input, output, CsppParser.statement)
    def ~/>(output: statement.type) = testBadInput(input, CsppParser.statement)

    def ~>(output: Seq[Statement]) = testGoodProgram(input, output)
    def ~/>(output: program.type) = testBadInput(input, CsppParser.program)
    def ~>(error: ParseError) = testBadProgram(input, error)
  }

  // Overload selectors for ~/> test
  object ident
  object component
  object expr
  object statement
  object program

  // Object used to state expectation for tests that should fail to parse
  class ParseError(line: Int, column: Int) extends Location(line, column)

  // Syntactic sugar for 0-argument functions (ie variables)
  def Var(id: Ident) = Application(id, Seq())

  // Syntactic sugar for application components
  def VarComponent(id: Ident, args: Seq[Expr]) = AppComponent(Application(id, args))

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Component tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  test("component.nullary") {
    // foo
    Seq(IDENT("foo")) ~> VarComponent(Ident("foo"), Seq())
  }

  test("component.oneary") {
    // foo(42)
    Seq(IDENT("foo"), LPAREN, NUMBER(42), RPAREN) ~> VarComponent(Ident("foo"), Seq(Num(42)))
  }

  test("component.polyary") {
    // foo(42, 43, 44)
    Seq(
      IDENT("foo"),
      LPAREN,
      NUMBER(42),
      COMMA,
      NUMBER(43),
      COMMA,
      NUMBER(44),
      RPAREN
    ) ~>
    VarComponent(Ident("foo"), Seq(Num(42), Num(43), Num(44)))
  }

  test("component.invalid.noCommas") {
    // foo(42 43 44)
    Seq(
      IDENT("foo"),
      LPAREN,
      NUMBER(42),
      NUMBER(43),
      NUMBER(44),
      RPAREN
    ) ~/> component
  }

  test("component.invalid.unmatchedLParen") {
    // foo(42
    Seq(
      IDENT("foo"),
      LPAREN,
      NUMBER(42)
    ) ~/> component
  }

  test("component.invalid.unmatchedRParen") {
    // foo 42)
    Seq(
      IDENT("foo"),
      NUMBER(42),
      RPAREN
    ) ~/> component
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Expression tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  test("expression.number") {
    Seq(NUMBER(42)) ~> Num(42)
  }

  test("expression.var") {
    Seq(IDENT("foo")) ~> Var(Ident("foo"))
  }

  test("expression.chain.empty") {
    Seq(LBRACE, RBRACE) ~> Chain(Seq())
  }

  test("expression.chain.one") {
    Seq(LBRACE, IDENT("foo"), RBRACE) ~> Chain(Seq(VarComponent(Ident("foo"), Seq())))
  }

  test("expressions.chain.many") {
    // { foo bar baz }
    Seq(
      LBRACE,
      IDENT("foo"),
      IDENT("bar"),
      IDENT("baz"),
      RBRACE
    ) ~> Chain(Seq(
      VarComponent(Ident("foo"), Seq()),
      VarComponent(Ident("bar"), Seq()),
      VarComponent(Ident("baz"), Seq())
    ))
  }

  test("expression.chain.oneWithArgs") {
    // { foo(42, 43, 44) }
    Seq(
      LBRACE,
        IDENT("foo"), LPAREN,
          NUMBER(42), COMMA,
          NUMBER(43), COMMA,
          NUMBER(44),
        RPAREN,
      RBRACE
    ) ~> Chain(Seq(VarComponent(Ident("foo"), Seq(Num(42), Num(43), Num(44)))))
  }

  test("expression.chain.manyWithArgs") {
    // { foo(42) bar(43) baz(42, 43) }
    Seq(
      LBRACE,
        IDENT("foo"), LPAREN, NUMBER(42), RPAREN,
        IDENT("bar"), LPAREN, NUMBER(43), RPAREN,
        IDENT("baz"), LPAREN,
          NUMBER(42), COMMA,
          NUMBER(43),
        RPAREN,
      RBRACE
    ) ~> Chain(Seq(
      VarComponent(Ident("foo"), Seq(Num(42))),
      VarComponent(Ident("bar"), Seq(Num(43))),
      VarComponent(Ident("baz"), Seq(Num(42), Num(43)))
    ))
  }

  test("expressions.chain.mixedArgs") {
    // { foo bar(41) }
    Seq(
      LBRACE,
        IDENT("foo"),
        IDENT("bar"), LPAREN, NUMBER(41), RPAREN,
      RBRACE
    ) ~> Chain(Seq(
      VarComponent(Ident("foo"), Seq()),
      VarComponent(Ident("bar"), Seq(Num(41)))
    ))
  }

  test("expressions.chain.invalid.noBraces") {
    // foo bar baz
    Seq(IDENT("foo"), IDENT("bar"), IDENT("baz")) ~/> expr
  }

  test("expressions.chain.invalid.unmatchedLBrace") {
    // { foo bar baz
    Seq(LBRACE, IDENT("foo"), IDENT("bar"), IDENT("baz")) ~/> expr
  }

  test("expressions.chain.invalid.unmatchedRBrace") {
    // foo bar baz }
    Seq(IDENT("foo"), IDENT("bar"), IDENT("baz"), RBRACE) ~/> expr
  }

  test("expressions.chain.invalid.commas") {
    // { foo, bar, baz }
    Seq(
      LBRACE,
        IDENT("foo"), COMMA,
        IDENT("bar"), COMMA,
        IDENT("baz"),
      RBRACE
    ) ~/> expr
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Statement tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  test("statement.assignment.ident") {
    // foo = bar
    Seq(IDENT("foo"), EQUALS, IDENT("bar")) ~> Assignment(Ident("foo"), Seq(), Var(Ident("bar")))
  }

  test("statement.assignment.number") {
    // foo = 42
    Seq(IDENT("foo"), EQUALS, NUMBER(42)) ~> Assignment(Ident("foo"), Seq(), Num(42))
  }

  test("statement.assignment.chain") {
    // foo = { bar }
    Seq(IDENT("foo"), EQUALS, LBRACE, IDENT("bar"), RBRACE) ~>
      Assignment(Ident("foo"), Seq(), Chain(Seq(VarComponent(Ident("bar"), Seq()))))
  }

  test("statement.assignment.chainWithArgs") {
    // foo(n) = { bar(n) }
    Seq(IDENT("foo"), LPAREN, IDENT("n"), RPAREN, EQUALS, LBRACE,
      IDENT("bar"), LPAREN, IDENT("n"), RPAREN,
    RBRACE) ~>
    Assignment(Ident("foo"), Seq(Ident("n")), Chain(Seq(VarComponent(Ident("bar"), Seq(Var(Ident("n")))))))
  }

  test("statement.assignment.invalid.chain") {
    // foo = bar baz bat
    Seq(IDENT("foo"), EQUALS, IDENT("bar"), IDENT("baz"), IDENT("bat")) ~/> statement
  }

  test("statement.assignment.invalid.undefined") {
    Seq(IDENT("foo")) ~/> statement
  }

  test("statement.assignment.invalid.incomplete") {
    Seq(IDENT("foo"), EQUALS) ~/> statement
  }

  test("statement.instrument.oneChannel") {
    // instr(1) = foo
    Seq(INSTR, LPAREN, NUMBER(1), RPAREN, EQUALS, IDENT("foo")) ~>
      Instrument(Seq(Num(1)), Var(Ident("foo")))
  }

  test("statement.instrument.manyChannels") {
    // instr(1, 3, channel) = { foo bar }
    Seq(
      INSTR, LPAREN,
        NUMBER(1), COMMA,
        NUMBER(3), COMMA,
        IDENT("channel"),
      RPAREN,
      EQUALS,
      LBRACE,
        IDENT("foo"),
        IDENT("bar"),
      RBRACE
    ) ~>
      Instrument(Seq(Num(1), Num(3), Var(Ident("channel"))), Chain(Seq(
        VarComponent(Ident("foo"), Seq()),
        VarComponent(Ident("bar"), Seq())
      )))
  }

  test("statement.instrument.invalid.noChannels") {
    // instr() = foo
    Seq(INSTR, LPAREN, RPAREN, EQUALS, IDENT("foo")) ~/> statement
  }

  test("statement.instrument.invalid.noParens") {
    // instr = foo
    Seq(INSTR, EQUALS, IDENT("foo")) ~/> statement
  }

  test("statement.instrument.invalid.unmatchedLParen") {
    // instr(1 = foo
    Seq(INSTR, LPAREN, NUMBER(1), EQUALS, IDENT("foo")) ~/> statement
  }

  test("statement.instrument.invalid.unmatchedRParen") {
    // instr 1) = foo
    Seq(INSTR, NUMBER(1), RPAREN, EQUALS, IDENT("foo")) ~/> statement
  }

  test("statement.instrument.invalid.noKeyword") {
    // (1) = foo
    Seq(LPAREN, NUMBER(1), RPAREN, EQUALS, IDENT("foo")) ~/> statement
  }

  test("statement.instrument.invalid.wrongKeyword") {
    // instrument(1) = foo
    Seq(IDENT("instrument"), LPAREN, NUMBER(1), RPAREN, EQUALS, IDENT("foo")) ~/> statement
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Import tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  test("import.resource") {
    Seq(IMPORT, FILE("test/library.csp")) ~> Seq(
      Assignment(Ident("lib_source"), Seq(Ident("freq"), Ident("amp")), Chain(Seq(
        AppComponent(Application(Ident("fm"), Seq(Var(Ident("freq")), Var(Ident("amp")), Num(4))))
      )))
    )
  }

  test("import.notFound") {
    Seq(IMPORT, FILE("test/nosuchfile.csp")) ~/> program
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Program tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  def programTest[T](program: String, verify: Seq[CsppToken] => T) = CsppLexer(program) match {
    case Right(output)                            => verify(output)
    case Left(CsppCompileError(reportedLoc, msg)) =>
      fail("Lexer error in parser test program. Possible regression in unit tests." ++
           reportedLoc.toString ++ ": " ++ msg)
  }

  implicit class ProgramTester(program: String) {
    def ~>(result: Seq[Statement]) = programTest(program, _ ~> result)
    def ~>(result: ParseError) = programTest(program, _ ~> result)
  }

  test("program.empty") {
    Seq() ~> Seq()
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
    """ ~>
    Seq(
      Assignment(Ident("foo"), Seq(), Num(1)),
      Assignment(Ident("bar"), Seq(), Var(Ident("foo"))),
      Assignment(Ident("source"), Seq(), Chain(Seq(
        VarComponent(Ident("fm"), Seq(Var(Ident("bar")), Num(440), Num(10)))
      ))),
      Assignment(Ident("effect"), Seq(), Chain(Seq(
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
    """ ~>
    new ParseError(1, 1)
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
    |instr(foo) source
    |
    |instr(2, 3) = {
    |  source
    |  effect
    |}
    """.stripMargin ~>
    new ParseError(13, 12)
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
    |  effect""".stripMargin ~>
    new ParseError(20, 3)
  }
}
