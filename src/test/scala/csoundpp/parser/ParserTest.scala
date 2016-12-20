package cspp

import org.scalatest.FunSuite
import org.scalatest.Matchers
import scala.util.parsing.input.{Position,NoPosition}

import absyn._
import AbsynSugar._
import tokens._

// Packrat parsers get really, REALLY() pissed off if the position is not consistent. Normally the
// lexer would fix the position for all of the tokens it outputs, but a lot of our tests don't run
// through the lexer; they just feed tokens directly into the parser. For these tests, we initialize
// the positions of each token with an instance of one of these guys.
case class MockPosition(offset: Int) extends Position {
  def line = 0
  def column = offset
  def lineContents = ""
}

class ParserSuite extends FunSuite with Matchers {

  // Assert that an input parses successfully and gives correct output.
  def testGoodInput(input: Seq[CsppToken], output: Statement) = {
    val parser = new CsppParser()
    val reader = parser.reader(input)
    val result = parser.phrase(parser.statement)(reader)
    result shouldBe 'successful
    result.get should equal (output)
  }

  def testGoodInput(input: Seq[CsppToken], output: Expr) = {
    val parser = new CsppParser()
    val reader = parser.reader(input)
    val result = parser.phrase(parser.expr)(reader)
    result shouldBe 'successful
    result.get should equal (output)
  }

  def testGoodInput(input: Seq[CsppToken], output: Ident) = {
    val parser = new CsppParser()
    val reader = parser.reader(input)
    val result = parser.phrase(parser.id)(reader)
    result shouldBe 'successful
    result.get should equal (output)
  }

  // Assert that an input failes to parse.
  def testBadInput[T](input: Seq[CsppToken], parser: statement.type) = {
    val parser = new CsppParser()
    val reader = parser.reader(input)
    val result = parser.phrase(parser.statement)(reader)
    result should not be 'successful
  }

  def testBadInput[T](input: Seq[CsppToken], parser: expr.type) = {
    val parser = new CsppParser()
    val reader = parser.reader(input)
    val result = parser.phrase(parser.expr)(reader)
    result should not be 'successful
  }

  def testBadInput[T](input: Seq[CsppToken], parser: ident.type) = {
    val parser = new CsppParser()
    val reader = parser.reader(input)
    val result = parser.phrase(parser.id)(reader)
    result should not be 'successful
  }

  def testBadInput[T](input: Seq[CsppToken], parser: program.type) = {
    CsppParser(input) match {
      case Right(output) => fail(output.toString ++ " was successful.")
      case Left(_)       => succeed
    }
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
  def testBadProgram(input: Seq[CsppToken], error: ParseError) = CsppParser(input) match {
    case Right(output)                          => fail(output.toString ++ " was successful.")
    case Left(CsppCompileError(reportedLoc, _)) => {
      (reportedLoc.line, reportedLoc.column) should equal ((error.line, error.column))
    }
  }

  implicit class ParseTester(input: Seq[CsppToken]) {

    for (i <- 0 to (input.length - 1)) {
      if (input(i).pos == NoPosition) {
        input(i).pos = MockPosition(i)
      }
    }

    def ~>(output: Ident) = testGoodInput(input, output)
    def ~/>(output: ident.type) = testBadInput(input, output)

    def ~>(output: Expr) = testGoodInput(input, output)
    def ~/>(output: expr.type) = testBadInput(input, output)

    def ~>(output: Statement) = testGoodInput(input, output)
    def ~/>(output: statement.type) = testBadInput(input, output)

    def ~>(output: Seq[Statement]) = testGoodProgram(input, output)
    def ~/>(output: program.type) = testBadInput(input, output)
    def ~>(error: ParseError) = testBadProgram(input, error)
  }

  // Overload selectors for ~/> test
  object ident
  object expr
  object statement
  object program

  // Object used to state expectation for tests that should fail to parse
  class ParseError(line: Int, column: Int) extends Location(line, column, "")

  // Syntactic sugar for 0-argument functions (ie variables)
  def Var(id: Ident) = Application(id, Seq())

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Expression tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  test("expression.number") {
    Seq(NUMBER(42)) ~> Num(42)
  }

  test("expression.var") {
    Seq(IDENT("foo")) ~> Var(Ident("foo"))
  }

  test("expression.function.zeroary") {
    // foo()
    Seq(IDENT("foo"), LPAREN(), RPAREN()) ~>
      Application(Ident("foo"), Seq())
  }

  test("expression.function.oneary") {
    // foo(1)
    Seq(IDENT("foo"), LPAREN(), NUMBER(1), RPAREN()) ~>
      Application(Ident("foo"), Seq(Num(1)))
  }

  test("expression.function.polyary") {
    // foo(arg1, 1)
    Seq(IDENT("foo"), LPAREN(), NUMBER(1), COMMA(), NUMBER(1), RPAREN()) ~>
      Application(Ident("foo"), Seq(Num(1), Num(1)))
  }

  test("expression.chain.empty") {
    Seq(LBRACE(), RBRACE()) ~> Chain(Seq())
  }

  test("expression.chain.one") {
    Seq(LBRACE(), IDENT("foo"), RBRACE()) ~> Chain(Seq(Application(Ident("foo"), Seq())))
  }

  test("expressions.chain.many") {
    // { foo bar baz }
    Seq(
      LBRACE(),
      IDENT("foo"),
      IDENT("bar"),
      IDENT("baz"),
      RBRACE()
    ) ~> Chain(Seq(
      Application(Ident("foo"), Seq()),
      Application(Ident("bar"), Seq()),
      Application(Ident("baz"), Seq())
    ))
  }

  test("expression.chain.oneWithArgs") {
    // { foo(42, 43, 44) }
    Seq(
      LBRACE(),
        IDENT("foo"), LPAREN(),
          NUMBER(42), COMMA(),
          NUMBER(43), COMMA(),
          NUMBER(44),
        RPAREN(),
      RBRACE()
    ) ~> Chain(Seq(Application(Ident("foo"), Seq(Num(42), Num(43), Num(44)))))
  }

  test("expression.chain.manyWithArgs") {
    // { foo(42) bar(43) baz(42, 43) }
    Seq(
      LBRACE(),
        IDENT("foo"), LPAREN(), NUMBER(42), RPAREN(),
        IDENT("bar"), LPAREN(), NUMBER(43), RPAREN(),
        IDENT("baz"), LPAREN(),
          NUMBER(42), COMMA(),
          NUMBER(43),
        RPAREN(),
      RBRACE()
    ) ~> Chain(Seq(
      Application(Ident("foo"), Seq(Num(42))),
      Application(Ident("bar"), Seq(Num(43))),
      Application(Ident("baz"), Seq(Num(42), Num(43)))
    ))
  }

  test("expressions.chain.mixedArgs") {
    // { foo bar(41) }
    Seq(
      LBRACE(),
        IDENT("foo"),
        IDENT("bar"), LPAREN(), NUMBER(41), RPAREN(),
      RBRACE()
    ) ~> Chain(Seq(
      Application(Ident("foo"), Seq()),
      Application(Ident("bar"), Seq(Num(41)))
    ))
  }

  test("expressions.chain.parallelComponent") {
    // { foo parallel { bar baz } }
    Seq(
      LBRACE(),
        IDENT("foo"),
        PARALLEL(), LBRACE(),
          IDENT("bar"),
          IDENT("baz"),
        RBRACE(),
      RBRACE()
    ) ~> Chain(Seq(
      Var("foo"),
      Parallel(Seq(
        Var("bar"),
        Var("baz")
      ))
    ))
  }

  test("expressions.chain.invalid.noBraces") {
    // foo bar baz
    Seq(IDENT("foo"), IDENT("bar"), IDENT("baz")) ~/> expr
  }

  test("expressions.chain.invalid.unmatchedLB()race") {
    // { foo bar baz
    Seq(LBRACE(), IDENT("foo"), IDENT("bar"), IDENT("baz")) ~/> expr
  }

  test("expressions.chain.invalid.unmatchedRB()race") {
    // foo bar baz }
    Seq(IDENT("foo"), IDENT("bar"), IDENT("baz"), RBRACE()) ~/> expr
  }

  test("expressions.chain.invalid.commas") {
    // { foo, bar, baz }
    Seq(
      LBRACE(),
        IDENT("foo"), COMMA(),
        IDENT("bar"), COMMA(),
        IDENT("baz"),
      RBRACE()
    ) ~/> expr
  }

  test("expression.parallel.empty") {
    Seq(PARALLEL(), LBRACE(), RBRACE()) ~> Parallel(Seq())
  }

  test("expression.parallel.one") {
    Seq(PARALLEL(), LBRACE(), IDENT("foo"), RBRACE()) ~> Parallel(Seq(Var("foo")))
  }

  test("expressions.parallel.many") {
    // parallel { foo bar baz }
    Seq(
      PARALLEL(), LBRACE(),
      IDENT("foo"),
      IDENT("bar"),
      IDENT("baz"),
      RBRACE()
    ) ~> Parallel(Seq(
      Application(Ident("foo"), Seq()),
      Application(Ident("bar"), Seq()),
      Application(Ident("baz"), Seq())
    ))
  }

  test("expression.parallel.oneWithArgs") {
    // parallel { foo(42, 43, 44) }
    Seq(
      PARALLEL(), LBRACE(),
        IDENT("foo"), LPAREN(),
          NUMBER(42), COMMA(),
          NUMBER(43), COMMA(),
          NUMBER(44),
        RPAREN(),
      RBRACE()
    ) ~> Parallel(Seq(Application(Ident("foo"), Seq(Num(42), Num(43), Num(44)))))
  }

  test("expression.parallel.manyWithArgs") {
    // parallel { foo(42) bar(43) baz(42, 43) }
    Seq(
      PARALLEL(), LBRACE(),
        IDENT("foo"), LPAREN(), NUMBER(42), RPAREN(),
        IDENT("bar"), LPAREN(), NUMBER(43), RPAREN(),
        IDENT("baz"), LPAREN(),
          NUMBER(42), COMMA(),
          NUMBER(43),
        RPAREN(),
      RBRACE()
    ) ~> Parallel(Seq(
      Application(Ident("foo"), Seq(Num(42))),
      Application(Ident("bar"), Seq(Num(43))),
      Application(Ident("baz"), Seq(Num(42), Num(43)))
    ))
  }

  test("expressions.parallel.mixedArgs") {
    // parallel { foo bar(41) }
    Seq(
      PARALLEL(), LBRACE(),
        IDENT("foo"),
        IDENT("bar"), LPAREN(), NUMBER(41), RPAREN(),
      RBRACE()
    ) ~> Parallel(Seq(
      Application(Ident("foo"), Seq()),
      Application(Ident("bar"), Seq(Num(41)))
    ))
  }

  test("expressions.parallel.chainComponent") {
    // parallel { foo {bar baz} }
    Seq(
      PARALLEL(), LBRACE(),
        IDENT("foo"),
        LBRACE(),
          IDENT("bar"),
          IDENT("baz"),
        RBRACE(),
      RBRACE()
    ) ~> Parallel(Seq(
      Var("foo"),
      Chain(Seq(
        Var("bar"),
        Var("baz")
      ))
    ))
  }

  test("expressions.parallel.invalid.noBraces") {
    // parallel foo bar baz
    Seq(PARALLEL(), IDENT("foo"), IDENT("bar"), IDENT("baz")) ~/> expr
  }

  test("expressions.parallel.invalid.unmatchedLBrace") {
    // parallel { foo bar baz
    Seq(PARALLEL(), LBRACE(), IDENT("foo"), IDENT("bar"), IDENT("baz")) ~/> expr
  }

  test("expressions.parallel.invalid.unmatchedRBrace") {
    // parallel foo bar baz }
    Seq(PARALLEL(), IDENT("foo"), IDENT("bar"), IDENT("baz"), RBRACE()) ~/> expr
  }

  test("expressions.parallel.invalid.commas") {
    // parallel { foo, bar, baz }
    Seq(
      PARALLEL(), LBRACE(),
        IDENT("foo"), COMMA(),
        IDENT("bar"), COMMA(),
        IDENT("baz"),
      RBRACE()
    ) ~/> expr
  }

  test("expressions.let.oneBinding") {
    // let x = 5 in y
    Seq(LET(), IDENT("x"), EQUALS(), NUMBER(5), IN(), IDENT("y")) ~>
      Let(Seq(Assignment("x", Seq(), Num(5))), Var("y"))
  }

  test("expressions.let.manyBindings") {
    // let { x = 5 y = 6 } in z
    Seq(LET(),
      LBRACE(), IDENT("x"), EQUALS(), NUMBER(5), IDENT("y"), EQUALS(), NUMBER(6), RBRACE(),
    IN(),
      IDENT("z")) ~>
    Let(Seq(Assignment("x", Seq(), Num(5)), Assignment("y", Seq(), Num(6))), Var("z"))
  }

  test("expressions.let.function") {
    // let s(x) = x + 1 in s(5)
    Seq(LET(),
      IDENT("s"), LPAREN(), IDENT("x"), RPAREN(), EQUALS(), IDENT("x"), PLUS(), NUMBER(1),
    IN(),
      IDENT("s"), LPAREN(), NUMBER(5), RPAREN()) ~>
    Let(Seq(Assignment("s", Seq("x"), BinOp(Var("x"), Plus, Num(1)))), Application("s", Seq(Num(5))))
  }

  test("expressions.let.invalid.noBinding") {
    // let in x
    Seq(LET(), IN(), IDENT("x")) ~/> expr
  }

  test("expressions.let.invalid.noBody.1") {
    // let x = 5
    Seq(LET(), IDENT("x"), EQUALS(), NUMBER(5)) ~/> expr
  }

  test("expressions.let.invalid.noBody.2") {
    // let x = 5 in
    Seq(LET(), IDENT("x"), EQUALS(), NUMBER(5), IN()) ~/> expr
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Arithmetic tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  test("arithmetic.term.mult") {
    // 42 * i
    Seq(NUMBER(42), STAR(), IDENT("i")) ~> BinOp(Num(42), Times, Application(Ident("i"), Seq()))
  }

  test("arithmetic.term.div") {
    // 42 / i
    Seq(NUMBER(42), SLASH(), IDENT("i")) ~> BinOp(Num(42), Divide, Application(Ident("i"), Seq()))
  }

  test("arithmetic.expr.add") {
    // 42 + i
    Seq(NUMBER(42), PLUS(), IDENT("i")) ~> BinOp(Num(42), Plus, Application(Ident("i"), Seq()))
  }

  test("arithmetic.expr.sub") {
    // 42 + i
    Seq(NUMBER(42), MINUS(), IDENT("i")) ~> BinOp(Num(42), Minus, Application(Ident("i"), Seq()))
  }

  test("arithmetic.associativity.mult") {
    // 1 * 2 * 3
    Seq(NUMBER(1), STAR(), NUMBER(2), STAR(), NUMBER(3)) ~>
      BinOp(BinOp(Num(1), Times, Num(2)), Times, Num(3))
  }

  test("arithmetic.associativity.div") {
    // 1 / 2 / 3
    Seq(NUMBER(1), SLASH(), NUMBER(2), SLASH(), NUMBER(3)) ~>
      BinOp(BinOp(Num(1), Divide, Num(2)), Divide, Num(3))
  }

  test("arithmetic.associativity.divMul") {
    // 1 / 2 * 3
    Seq(NUMBER(1), SLASH(), NUMBER(2), STAR(), NUMBER(3)) ~>
      BinOp(BinOp(Num(1), Divide, Num(2)), Times, Num(3))
  }

  test("arithmetic.associativity.mulDiv") {
    // 1 * 2 / 3
    Seq(NUMBER(1), STAR(), NUMBER(2), SLASH(), NUMBER(3)) ~>
      BinOp(BinOp(Num(1), Times, Num(2)), Divide, Num(3))
  }


  test("arithmetic.associativity.add") {
    // 1 + 2 + 3
    Seq(NUMBER(1), PLUS(), NUMBER(2), PLUS(), NUMBER(3)) ~>
      BinOp(BinOp(Num(1), Plus, Num(2)), Plus, Num(3))
  }

  test("arithmetic.associativity.sub") {
    // 1 - 2 - 3
    Seq(NUMBER(1), MINUS(), NUMBER(2), MINUS(), NUMBER(3)) ~>
      BinOp(BinOp(Num(1), Minus, Num(2)), Minus, Num(3))
  }

  test("arithmetic.associativity.subAdd") {
    // 1 - 2 + 3
    Seq(NUMBER(1), MINUS(), NUMBER(2), PLUS(), NUMBER(3)) ~>
      BinOp(BinOp(Num(1), Minus, Num(2)), Plus, Num(3))
  }

  test("arithmetic.associativity.addSub") {
    // 1 + 2 - 3
    Seq(NUMBER(1), PLUS(), NUMBER(2), MINUS(), NUMBER(3)) ~>
      BinOp(BinOp(Num(1), Plus, Num(2)), Minus, Num(3))
  }

  test("arithmetic.precedence.addMul") {
    // 1 + 2 * 3
    Seq(NUMBER(1), PLUS(), NUMBER(2), STAR(), NUMBER(3)) ~>
      BinOp(Num(1), Plus, BinOp(Num(2), Times, Num(3)))
  }

  test("arithmetic.precedence.subDiv") {
    // 1 - 2 / 3
    Seq(NUMBER(1), MINUS(), NUMBER(2), SLASH(), NUMBER(3)) ~>
      BinOp(Num(1), Minus, BinOp(Num(2), Divide, Num(3)))
  }

  test("arithmetic.precedence.parenthetical") {
    // (1 + 2) * 3
    Seq(LPAREN(), NUMBER(1), PLUS(), NUMBER(2), RPAREN(), STAR(), NUMBER(3)) ~>
      BinOp(BinOp(Num(1), Plus, Num(2)), Times, Num(3))
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Statement tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  test("statement.assignment.ident") {
    // foo = bar
    Seq(IDENT("foo"), EQUALS(), IDENT("bar")) ~> Assignment(Ident("foo"), Seq(), Var(Ident("bar")))
  }

  test("statement.assignment.number") {
    // foo = 42
    Seq(IDENT("foo"), EQUALS(), NUMBER(42)) ~> Assignment(Ident("foo"), Seq(), Num(42))
  }

  test("statement.assignment.chain") {
    // foo = { bar }
    Seq(IDENT("foo"), EQUALS(), LBRACE(), IDENT("bar"), RBRACE()) ~>
      Assignment(Ident("foo"), Seq(), Chain(Seq(Application(Ident("bar"), Seq()))))
  }

  test("statement.assignment.chainWithArgs") {
    // foo(n) = { bar(n) }
    Seq(IDENT("foo"), LPAREN(), IDENT("n"), RPAREN(), EQUALS(), LBRACE(),
      IDENT("bar"), LPAREN(), IDENT("n"), RPAREN(),
    RBRACE()) ~>
    Assignment(Ident("foo"), Seq(Ident("n")), Chain(Seq(Application(Ident("bar"), Seq(Var(Ident("n")))))))
  }

  test("statement.assignment.invalid.chain") {
    // foo = bar baz bat
    Seq(IDENT("foo"), EQUALS(), IDENT("bar"), IDENT("baz"), IDENT("bat")) ~/> statement
  }

  test("statement.assignment.invalid.undefined") {
    Seq(IDENT("foo")) ~/> statement
  }

  test("statement.assignment.invalid.incomplete") {
    Seq(IDENT("foo"), EQUALS()) ~/> statement
  }

  test("statement.instrument.oneChannel") {
    // instr(1) = foo
    Seq(INSTR(), LPAREN(), NUMBER(1), RPAREN(), EQUALS(), IDENT("foo")) ~>
      Instrument(Seq(Num(1)), Var(Ident("foo")))
  }

  test("statement.instrument.manyChannels") {
    // instr(1, 3, channel) = { foo bar }
    Seq(
      INSTR(), LPAREN(),
        NUMBER(1), COMMA(),
        NUMBER(3), COMMA(),
        IDENT("channel"),
      RPAREN(),
      EQUALS(),
      LBRACE(),
        IDENT("foo"),
        IDENT("bar"),
      RBRACE()
    ) ~>
      Instrument(Seq(Num(1), Num(3), Var(Ident("channel"))), Chain(Seq(
        Application(Ident("foo"), Seq()),
        Application(Ident("bar"), Seq())
      )))
  }

  test("statement.instrument.inserts") {
    // instr(1) = foo inserts bar
    Seq(INSTR(), LPAREN(), NUMBER(1), RPAREN(), EQUALS(), IDENT("foo"), INSERTS(), IDENT("bar")) ~>
      Instrument(Seq(Num(1)), Chain(Seq(Var("foo"), Var("bar"))))
  }

  test("statement.instrument.sends") {
    // instr(1) = foo sends bar
    Seq(INSTR(), LPAREN(), NUMBER(1), RPAREN(), EQUALS(), IDENT("foo"), SENDS(), IDENT("bar")) ~>
      Instrument(Seq(Num(1)), Var("foo"), Some(Var("bar")))
  }

  test("statement.instruments.insertsAndSends") {
    // instr(1) = foo inserts bar sends baz
    Seq(INSTR(), LPAREN(), NUMBER(1), RPAREN(), EQUALS(), IDENT("foo"),
      INSERTS(), IDENT("bar"), SENDS(), IDENT("baz")) ~>
      Instrument(Seq(Num(1)), Chain(Seq(Var("foo"), Var("bar"))), Some(Var("baz")))
  }

  test("statement.instrument.invalid.noChannels") {
    // instr() = foo
    Seq(INSTR(), LPAREN(), RPAREN(), EQUALS(), IDENT("foo")) ~/> statement
  }

  test("statement.instrument.invalid.noParens") {
    // instr = foo
    Seq(INSTR(), EQUALS(), IDENT("foo")) ~/> statement
  }

  test("statement.instrument.invalid.unmatchedLP()aren") {
    // instr(1 = foo
    Seq(INSTR(), LPAREN(), NUMBER(1), EQUALS(), IDENT("foo")) ~/> statement
  }

  test("statement.instrument.invalid.unmatchedRP()aren") {
    // instr 1) = foo
    Seq(INSTR(), NUMBER(1), RPAREN(), EQUALS(), IDENT("foo")) ~/> statement
  }

  test("statement.instrument.invalid.noKeyword") {
    // (1) = foo
    Seq(LPAREN(), NUMBER(1), RPAREN(), EQUALS(), IDENT("foo")) ~/> statement
  }

  test("statement.instrument.invalid.wrongKeyword") {
    // instrument(1) = foo
    Seq(IDENT("instrument"), LPAREN(), NUMBER(1), RPAREN(), EQUALS(), IDENT("foo")) ~/> statement
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Import tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  test("import.resource") {
    Seq(IMPORT(), FILE("test/library.csp")) ~> Seq(
      Assignment(Ident("lib_source"), Seq(Ident("freq"), Ident("amp")), Chain(Seq(
        Application(Ident("fm"), Seq(Var(Ident("freq")), Var(Ident("amp")), Num(4)))
      )))
    )
  }

  test("import.twice") {
    Seq(IMPORT(), FILE("test/library.csp"), IMPORT(), FILE("test/library.csp")) ~> Seq(
      Assignment(Ident("lib_source"), Seq(Ident("freq"), Ident("amp")), Chain(Seq(
        Application(Ident("fm"), Seq(Var(Ident("freq")), Var(Ident("amp")), Num(4)))
      )))
    )
  }

  test("import.notFound") {
    Seq(IMPORT(), FILE("test/nosuchfile.csp")) ~/> program
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
        Application(Ident("fm"), Seq(Var(Ident("bar")), Num(440), Num(10)))
      ))),
      Assignment(Ident("effect"), Seq(), Chain(Seq(
        Application(Ident("compress"), Seq(Num(40), Var(Ident("bar"))))
      ))),
      Instrument(Seq(Var(Ident("foo"))), Var(Ident("source"))),
      Instrument(Seq(Num(2), Num(3)), Chain(Seq(
        Application(Ident("source"), Seq()),
        Application(Ident("effect"), Seq())
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
    new ParseError(20, 4)
  }
}
