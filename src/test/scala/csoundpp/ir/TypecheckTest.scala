package cspp

import scala.language.implicitConversions
import org.scalatest.FunSuite
import org.scalatest.Matchers

import absyn._
import tokens._

class TypecheckSuite extends FunSuite with Matchers {

  type Env = CsppTypeChecker.Env

  // Syntactic sugar for function types
  implicit class ZeroAry(resultTy: CsppType) extends Function(resultTy, 0)
  implicit class PolyAry(input: (CsppType, Int)) extends Function(input._1, input._2)

  def env(mappings: (String, Function)*): Env =
    if (mappings.isEmpty) {
      new Env()
    } else {
      env(mappings.tail:_*) + (Ident(mappings.head._1) -> mappings.head._2)
    }

  // Syntactic sugar for 0-argument functions (ie variables)
  def Var(id: Ident) = Application(id, Seq())
  def Assign(id: Ident, body: Expr) = Assignment(id, Seq(), body)

  def tryGoodElement[T](env: Env, input: T, expected: T, annotator: (Env, T) => T) = {
    annotator(env, input) should equal (expected)
  }

  def tryBadElement[T](env: Env, input: T, expected: TypeError, annotator: (Env, T) => T) = {
    try {
      fail(annotator(env, input).toString ++ " was successful.")
    } catch {
      case CsppCompileError(loc, _) => {
        loc.line should equal (expected.line)
        loc.column should equal (expected.column)
      }
    }
  }

  def tryBadElement[T](env: Env, input: T, annotator: (Env, T) => T) = {
    try {
      fail(annotator(env, input).toString ++ " was successful.")
    } catch {
      case CsppCompileError(loc, _) => ()
    }
  }

  def testGoodInput(env: Env, input: Seq[Statement], expected: Seq[Statement]) = {
    CsppTypeChecker(env, input) match {
      case Right(output) => {
        output should equal (expected)      }
      case Left(err) => fail(input.toString ++ " failed to typecheck: " ++ err.toString)
    }
  }

  def testBadInput(env: Env, input: Seq[Statement], loc: Location) =
  CsppTypeChecker(env, input) match {
    case Right(output)                          => fail(output.toString ++ " was successful.")
    case Left(CsppCompileError(reportedLoc, _)) => reportedLoc should equal (loc)
  }

  def testBadInput(env: Env, input: Seq[Statement]) = CsppTypeChecker(env, input) match {
    case Right(output)  => fail(output.toString ++ " was successful.")
    case Left(_)        => ()
  }

  // Test a whole program starting from a string, and going through the lexing, parsing, and
  // typecheck phases. This is used to test the locations reported with type errors
  class SystemTester(env: Env, input: String) {
    def lexAndThen[T](continue: Seq[CsppToken] => T)(input: String) = {
      CsppLexer(input) match {
        case Right(output)                            => continue(output)
        case Left(CsppCompileError(reportedLoc, msg)) =>
          fail("Lexer error in typecheck test program. Possible regression in unit tests." ++
               reportedLoc.toString ++ ": " ++ msg)
      }
    }

    def parseAndThen[T](continue: Seq[Statement] => T)(input: Seq[CsppToken]) = {
      CsppParser(input) match {
        case Right(output)                            => continue(output)
        case Left(CsppCompileError(reportedLoc, msg)) =>
          fail("Parse error in typecheck test program. Possible regression in unit tests." ++
               reportedLoc.toString ++ ": " ++ msg)
      }
    }

    def ~>(output: Seq[Statement]) =
      lexAndThen {
        parseAndThen { (ast: Seq[Statement]) =>
          env ~> ast ~> output
        }
      } (input)

    def ~>(error: TypeError) =
      lexAndThen {
        parseAndThen { (ast: Seq[Statement]) =>
          env ~> ast ~> error
        }
      } (input)
  }

  class ProgramTester(val env: Env, val input: Seq[Statement]) {
    def ~>(output: Seq[Statement]) = testGoodInput(env, input, output)
    def ~>(error: TypeError) = testBadInput(env, input, error)
    def ~/>(output: ExpectFailure) = testBadInput(env, input)
  }

  class ElemTester[T](val env: Env, val input: T, val annotator: (Env, T) => T) {
    def ~>(output: T) = tryGoodElement(env, input, output, annotator)
    def ~>(error: TypeError) = tryBadElement(env, input, error, annotator)
    def ~/>(output: ExpectFailure) = tryBadElement(env, input, annotator)
  }

  class StmtTester(env: Env, input: Statement)
    extends ElemTester(env, input, (
      (env: Env, stmt: Statement) => CsppTypeChecker.annotateStmt(env, stmt)._2))

  class ExprTester(env: Env, input: Expr)
    extends ElemTester(env, input, CsppTypeChecker.annotateExpr)

  implicit class TypecheckTestBuilder(env: Env) {
    def ~>(input: Seq[Statement]) = new ProgramTester(env, input)
    def ~>(input: Statement) = new StmtTester(env, input)
    def ~>(input: Expr) = new ExprTester(env, input)
    def ~>(input: String) = new SystemTester(env, input)
  }

  // Overload selectors for tests that expect the type check to fail
  class ExpectFailure
  object ident extends ExpectFailure
  object expr extends ExpectFailure
  object statement extends ExpectFailure

  // Object used to state expectation for tests that should fail the typecheck phase
  class TypeError(line: Int, column: Int) extends Location(line, column, "")

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Expression tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  test("expression.num") {
    val expr = Num(42)
    env() ~> expr ~> (expr annotated Number)
  }

  test("expression.var.num") {
    val expr = Var(Ident("foo"))
    env("foo" -> Number) ~> expr ~> (expr annotated Number)
  }

  test("expression.var.source") {
    val expr = Var(Ident("foo"))
    env("foo" -> Source) ~> expr ~> (expr annotated Source)
  }

  test("expression.var.effect") {
    val expr = Var(Ident("foo"))
    env("foo" -> Effect) ~> expr ~> (expr annotated Effect)
  }

  test("expression.var.invalid.unknown") {
    env() ~> Var(Ident("foo")) ~/> expr
  }

  test("expression.function.number") {
    val expr = Application(Ident("foo"), Seq(Num(42)))
    val annotated = Application(Ident("foo"), Seq(Num(42) annotated Number)) annotated Number
    env("foo" -> (Number, 1)) ~> expr ~> annotated
  }

  test("expression.function.invalid.nonnumericArgs") {
    env("foo" -> (Number, 1), "source" -> Source) ~>
      Application(Ident("foo"), Seq(Var("source"))) ~/> expr
  }

  test("expression.chain.empty") {
    env() ~> Chain(Seq()) ~> (Chain(Seq()) annotated Effect)
  }

  test("expression.chain.sourceOnly") {
    val comp = Application(Ident("source"), Seq())
    val expr = Chain(Seq(comp))
    val annotated = Chain(Seq(comp annotated Source)) annotated Source
    env("source" -> Source) ~> expr ~> annotated
  }

  test("expression.chain.effectOnly") {
    val comp = Application(Ident("effect"), Seq())
    val expr = Chain(Seq(comp))
    val annotated = Chain(Seq(comp annotated Effect)) annotated Effect
    env("effect" -> Effect) ~> expr ~> annotated
  }

  test("expression.chain.sourceAndEffect") {
    val source = Application(Ident("source"), Seq())
    val effect = Application(Ident("effect"), Seq())
    val expr = Chain(Seq(source, effect))
    val annotated = Chain(Seq(source annotated Source, effect annotated Effect)) annotated Source
    env("source" -> Source, "effect" -> Effect) ~> expr ~> annotated
  }

  test("expression.chain.invalid.twoSource") {
    val source1 = Application(Ident("source1"), Seq())
    val source2 = Application(Ident("source2"), Seq())
    val input = Chain(Seq(source1, source2))
    env("source1" -> Source, "source2" -> Source) ~> input ~/> expr
  }

  test("expression.chain.invalid.effectAndSource") {
    val source = Application(Ident("source"), Seq())
    val effect = Application(Ident("effect"), Seq())
    val input = Chain(Seq(effect, source))
    env("source" -> Source, "effect" -> Effect) ~> input ~/> expr
  }

  test("expression.chain.invalid.number") {
    env("var" -> Number) ~> Chain(Seq(Var(Ident("var")))) ~/> expr
  }

  test("expression.binop.numbers") {
    val lhs = Num(42)
    val rhs = Num(1)
    val expr = BinOp(lhs, Plus, rhs)
    val annotated = BinOp(lhs annotated Number, Plus, rhs annotated Number) annotated Number
    env() ~> expr ~> annotated
  }

  test("expression.binop.functions") {
    val lhs = Var(Ident("foo"))
    val rhs = Var(Ident("bar"))
    val expr = BinOp(lhs, Plus, rhs)
    val annotated = BinOp(lhs annotated Number, Plus, rhs annotated Number) annotated Number
    env("foo" -> Number, "bar" -> Number) ~> expr ~> annotated
  }

  test("expression.binop.invalid.source") {
    env("source" -> Source) ~> BinOp(Num(42), Plus, Var("source")) ~/> expr
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Statement tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  test("statement.assignment.num") {
    val expr = Num(42)
    val stmt = Assign(Ident("foo"), expr)
    val annotated = Assign(Ident("foo"), expr annotated Number)
    env() ~> stmt ~> annotated
  }

  test("statement.assignment.var") {
    val expr = Var(Ident("bar"))
    val stmt = Assign(Ident("foo"), expr)
    val annotated = Assign(Ident("foo"), expr annotated Number)
    env("bar" -> Number) ~> stmt ~> annotated
  }

  test("statement.assignment.chain") {
    val comp = Application(Ident("source"), Seq())
    val expr = Chain(Seq(comp))
    val stmt = Assign(Ident("foo"), expr)
    val annotated = Assign(Ident("foo"), Chain(Seq(comp annotated Source)) annotated Source)
    env("source" -> Source) ~> stmt ~> annotated
  }

  test("statement.assignment.chainWithParams") {
    val comp = Application(Ident("source"), Seq(Var(Ident("param"))))
    val expr = Chain(Seq(comp))
    val stmt = Assignment(Ident("foo"), Seq(Ident("param")), expr)
    val annotated = Assignment(Ident("foo"), Seq(Ident("param")),
      Chain(Seq(
        Application(
          Ident("source"),
          Seq(Var(Ident("param")) annotated Number)
        ) annotated Source
      )) annotated Source)
    env("source" -> (Source, 1)) ~> stmt ~> annotated
  }

  test("statement.assignment.env") {
    val expr = Num(42)
    val stmts = Seq(
      Assign(Ident("foo"), expr),
      Assign(Ident("bar"), Var(Ident("foo")))
    )
    val annotated = Seq(
      Assign(Ident("foo"), expr annotated Number),
      Assign(Ident("bar"), Var(Ident("foo")) annotated Number)
    )
    env() ~> stmts ~> annotated
  }

  test("statement.assignment.invalid.repeatedArg") {
    val name = Ident("foo")
    val param = Ident("param")
    val definition = Var(Ident("definition"))

    env("definition" -> Number) ~> Assignment(name, Seq(param, param), definition) ~/> statement
  }

  test("statement.assignment.invalid.unknownVar") {
    env() ~> Assign(Ident("foo"), Var(Ident("bar"))) ~/> statement
  }

  test("statement.assignment.invalid.reassignment") {
    env("foo" -> Number) ~> Assign(Ident("foo"), Num(42)) ~/> statement
  }

  test("statement.assignment.invalid.selfAssign.known") {
    env("foo" -> Number) ~> Assign(Ident("foo"), Var(Ident("foo"))) ~/> statement
  }

  test("statement.assignment.invalid.selfAssign.unknown") {
    env() ~> Assign(Ident("foo"), Var(Ident("foo"))) ~/> statement
  }

  test("statement.instrument.var") {
    val channel = Num(1)
    val body = Var(Ident("source"))
    val instr = Instrument(Seq(channel), body)
    val annotated = Instrument(Seq(channel annotated Number), body annotated Source)
    env("source" -> Source) ~> instr ~> annotated
  }

  test("statement.instrument.chain") {
    val channel = Num(1)
    val source = Application(Ident("source"), Seq())
    val effect = Application(Ident("effect"), Seq())
    val instr = Instrument(Seq(channel), Chain(Seq(source, effect)))
    val annotated = Instrument(
                      Seq(channel annotated Number),
                      Chain(Seq(
                        source annotated Source,
                        effect annotated Effect
                      ))
                    annotated Source)

    env("source" -> Source, "effect" -> Effect) ~> instr ~> annotated
  }

  test("statement.instrument.varChannel") {
    val channel = Var(Ident("channel"))
    val body = Var(Ident("source"))
    val instr = Instrument(Seq(channel), body)
    val annotated = Instrument(Seq(channel annotated Number), body annotated Source)
    env("source" -> Source, "channel" -> Number) ~> instr ~> annotated
  }

  def testMidiParam(paramName: String) = {
    val channel = Num(1)
    val param = Var(Ident(paramName))
    val source = Application(Ident("source"), Seq(param))
    val instr = Instrument(Seq(channel), Chain(Seq(source)))
    val annotated = Instrument(
                      Seq(channel annotated Number),
                      Chain(Seq(
                        Application(
                          Ident("source"),
                          Seq(param annotated Number))
                        annotated Source
                      ))
                    annotated Source)

    env("source" -> (Source, 1)) ~> instr ~> annotated
  }

  test("statement.instrument.midiParams.freq") {
    testMidiParam("freq")
  }

  test("statement.instrument.midiParams.amp") {
    testMidiParam("amp")
  }

  test("statement.instrument.invalid.illtypedChannel") {
    val channel = Var(Ident("source"))
    val body = Var(Ident("source"))
    val instr = Instrument(Seq(channel), body)
    env("source" -> Source) ~> instr ~/> statement
  }

  test("statement.instrument.invalid.number") {
    val channel = Num(1)
    val body = Num(2)
    val instr = Instrument(Seq(channel), body)
    env() ~> instr ~/> statement
  }

  test("statement.instrument.invalid.effectVar") {
    val channel = Num(1)
    val body = Var(Ident("effect"))
    val instr = Instrument(Seq(channel), body)
    env("effect" -> Effect) ~> instr ~/> statement
  }

  test("statement.instrument.invalid.effectChain") {
    val channel = Num(1)
    val body = Chain(Seq(Application(Ident("effect"), Seq())))
    val instr = Instrument(Seq(channel), body)
    env("effect" -> Effect) ~> instr ~/> statement
  }

  test("statement.instrument.invalid.unknownVar") {
    val channel = Num(1)
    val body = Var(Ident("foo"))
    val instr = Instrument(Seq(channel), body)
    env() ~> instr ~/> statement
  }


  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Program tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  test("program.valid") {
    env("fm" -> (Source, 3), "compress" -> (Effect, 2)) ~>
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
      Assign(Ident("foo"), Num(1) annotated Number),
      Assign(Ident("bar"), Var(Ident("foo")) annotated Number),
      Assign(Ident("source"),
        Chain(Seq(
          Application(Ident("fm"),
            Seq(
              Var(Ident("bar")) annotated Number,
              Num(440) annotated Number,
              Num(10) annotated Number
            ))
          annotated Source))
        annotated Source),
      Assign(Ident("effect"),
        Chain(Seq(
          Application(Ident("compress"),
            Seq(
              Num(40) annotated Number,
              Var(Ident("bar")) annotated Number
            ))
          annotated Effect))
        annotated Effect),
      Instrument(Seq(Var(Ident("foo")) annotated Number), Var(Ident("source")) annotated Source),
      Instrument(Seq(Num(2) annotated Number, Num(3) annotated Number),
        Chain(
          Seq(
            Application(Ident("source"), Seq()) annotated Source,
            Application(Ident("effect"), Seq()) annotated Effect
          ))
        annotated Source)
    )
  }

  test("program.invalid.firstLine") {
    env("fm" -> (Source, 3), "compress" -> (Effect, 2)) ~>
    """instr(1) = 1
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
    new TypeError(1, 12)
  }

  test("program.invalid.middle") {
    env("fm" -> (Source, 3), "compress" -> (Effect, 2)) ~>
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
    |instr(foo) = unknownVar
    |
    |instr(2, 3) = {
    |  source
    |  effect
    |}
    """.stripMargin ~>
    new TypeError(13, 14)
  }

  test("program.invalid.lastLine") {
    env("fm" -> (Source, 3), "compress" -> (Effect, 2)) ~>
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
    |  source}""".stripMargin ~>
    new TypeError(20, 3)
  }

}
