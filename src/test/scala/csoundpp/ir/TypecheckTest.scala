package cspp

import scala.language.implicitConversions
import org.scalatest.FunSuite
import org.scalatest.Matchers

class TypecheckSuite extends FunSuite with Matchers {

  type Env = CsppTypeChecker.Env

  def env(mappings: (String, CsppType)*): Env =
    if (mappings.isEmpty) {
      new Env()
    } else {
      env(mappings.tail:_*) + (Ident(mappings.head._1) -> mappings.head._2)
    }

  def tryGoodElement[T](env: Env, input: T, expected: T, annotator: (Env, T) => T) = {
    annotator(env, input) should equal (expected)
  }

  def tryBadElement[T](env: Env, input: T, expected: TypeError, annotator: (Env, T) => T) = {
    try {
      fail(annotator(env, input).toString ++ " was successful.")
    } catch {
      case CsppCompileError(loc, _) => loc should equal (expected)
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

  class CompTester(env: Env, input: Component)
    extends ElemTester(env, input, CsppTypeChecker.annotateComponent)

  implicit class TypecheckTestBuilder(env: Env) {
    def ~>(input: Seq[Statement]) = new ProgramTester(env, input)
    def ~>(input: Statement) = new StmtTester(env, input)
    def ~>(input: Expr) = new ExprTester(env, input)
    def ~>(input: Component) = new CompTester(env, input)
    def ~>(input: String) = new SystemTester(env, input)
  }

  // Overload selectors for tests that expect the type check to fail
  class ExpectFailure
  object ident extends ExpectFailure
  object component extends ExpectFailure
  object expr extends ExpectFailure
  object statement extends ExpectFailure

  implicit class ExprAnnotator(expr: Expr) {
    def annotated(ty: CsppType): Expr = expr match {
      case Num(n, _)              => Num(n, Some(ty))
      case Var(i, _)              => Var(i, Some(ty))
      case Chain(b, _)            => Chain(b, Some(ty))
    }
  }

  implicit class CompAnnotator(comp: Component) {
    def annotated(ty: CsppType): Component = comp match {
      case VarComponent(i, a, _)  => VarComponent(i, a, Some(ty))
    }
  }

  // Object used to state expectation for tests that should fail the typecheck phase
  class TypeError(line: Int, column: Int) extends Location(line, column)

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Component tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  test("component.var.source.noArgs") {
    val comp = VarComponent(Ident("source"), Seq())
    env("source" -> Source) ~> comp ~> (comp annotated Source)
  }

  test("component.var.source.args") {
    val args = Seq(Num(1), Var(Ident("num")))
    val annotatedArgs = args map ((e: Expr) => e annotated Number)
    val comp = VarComponent(Ident("source"), args)
    val annotatedComp = VarComponent(Ident("source"), annotatedArgs, Some(Source))
    env("source" -> Source, "num" -> Number) ~> comp ~> annotatedComp
  }

  test("component.var.effect.noArgs") {
    val comp = VarComponent(Ident("effect"), Seq())
    env("effect" -> Effect) ~> comp ~> (comp annotated Effect)
  }

  test("component.var.effect.args") {
    val args = Seq(Num(1), Var(Ident("num")))
    val annotatedArgs = args map ((e: Expr) => e annotated Number)
    val comp = VarComponent(Ident("effect"), annotatedArgs)
    val annotatedComp = comp annotated Effect
    env("effect" -> Effect, "num" -> Number) ~> comp ~> annotatedComp
  }

  test("component.var.invalid.unknownID") {
    env() ~> VarComponent(Ident("source"), Seq()) ~/> component
  }

  test("component.var.invalid.unknownArg") {
    val args = Seq(Var(Ident("arg")))
    val comp = VarComponent(Ident("source"), args)
    env("source" -> Source) ~> comp ~/> component
  }

  test("component.var.invalid.wrongTypeArgs") {
    val args = Seq(Var(Ident("arg")))
    val comp = VarComponent(Ident("source"), args)
    env("source" -> Source, "arg" -> Source) ~> comp ~/> component
  }

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

  test("expression.chain.sourceOnly") {
    val comp = VarComponent(Ident("source"), Seq())
    val expr = Chain(Seq(comp))
    val annotated = Chain(Seq(comp annotated Source)) annotated Source
    env("source" -> Source) ~> expr ~> annotated
  }

  test("expression.chain.effectOnly") {
    val comp = VarComponent(Ident("effect"), Seq())
    val expr = Chain(Seq(comp))
    val annotated = Chain(Seq(comp annotated Effect)) annotated Effect
    env("effect" -> Effect) ~> expr ~> annotated
  }

  test("expression.chain.sourceAndEffect") {
    val source = VarComponent(Ident("source"), Seq())
    val effect = VarComponent(Ident("effect"), Seq())
    val expr = Chain(Seq(source, effect))
    val annotated = Chain(Seq(source annotated Source, effect annotated Effect)) annotated Source
    env("source" -> Source, "effect" -> Effect) ~> expr ~> annotated
  }

  test("expression.chain.invalid.twoSource") {
    val source1 = VarComponent(Ident("source1"), Seq())
    val source2 = VarComponent(Ident("source2"), Seq())
    val input = Chain(Seq(source1, source2))
    env("source1" -> Source, "source2" -> Source) ~> input ~/> expr
  }

  test("expression.chain.invalid.effectAndSource") {
    val source = VarComponent(Ident("source"), Seq())
    val effect = VarComponent(Ident("effect"), Seq())
    val input = Chain(Seq(effect, source))
    env("source" -> Source, "effect" -> Effect) ~> input ~/> expr
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Statement tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  test("statement.assignment.num") {
    val expr = Num(42)
    val stmt = Assignment(Ident("foo"), expr)
    val annotated = Assignment(Ident("foo"), expr annotated Number)
    env() ~> stmt ~> annotated
  }

  test("statement.assignment.var") {
    val expr = Var(Ident("bar"))
    val stmt = Assignment(Ident("foo"), expr)
    val annotated = Assignment(Ident("foo"), expr annotated Number)
    env("bar" -> Number) ~> stmt ~> annotated
  }

  test("statement.assignment.chain") {
    val expr = Chain(Seq(VarComponent(Ident("source"), Seq())))
  }

  test("statement.assignment.env") {
    val expr = Num(42)
    val stmts = Seq(
      Assignment(Ident("foo"), expr),
      Assignment(Ident("bar"), Var(Ident("foo")))
    )
    val annotated = Seq(
      Assignment(Ident("foo"), expr annotated Number),
      Assignment(Ident("bar"), Var(Ident("foo")) annotated Number)
    )
    env() ~> stmts ~> annotated
  }

  test("statement.assignment.invalid.unknownVar") {
    env() ~> Assignment(Ident("foo"), Var(Ident("bar"))) ~/> statement
  }

  test("statement.assignment.invalid.reassignment") {
    env("foo" -> Number) ~> Assignment(Ident("foo"), Num(42)) ~/> statement
  }

  test("statement.assignment.invalid.selfAssign.known") {
    env("foo" -> Number) ~> Assignment(Ident("foo"), Var(Ident("foo"))) ~/> statement
  }

  test("statement.assignment.invalid.selfAssign.unknown") {
    env() ~> Assignment(Ident("foo"), Var(Ident("foo"))) ~/> statement
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
    val source = VarComponent(Ident("source"), Seq())
    val effect = VarComponent(Ident("effect"), Seq())
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
    val body = Chain(Seq(VarComponent(Ident("effect"), Seq())))
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
    env("fm" -> Source, "compress" -> Effect) ~>
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
      Assignment(Ident("foo"), Num(1) annotated Number),
      Assignment(Ident("bar"), Var(Ident("foo")) annotated Number),
      Assignment(Ident("source"),
        Chain(Seq(
          VarComponent(Ident("fm"),
            Seq(
              Var(Ident("bar")) annotated Number,
              Num(440) annotated Number,
              Num(10) annotated Number
            ))
          annotated Source))
        annotated Source),
      Assignment(Ident("effect"),
        Chain(Seq(
          VarComponent(Ident("compress"),
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
            VarComponent(Ident("source"), Seq()) annotated Source,
            VarComponent(Ident("effect"), Seq()) annotated Effect
          ))
        annotated Source)
    )
  }

  test("program.invalid.firstLine") {
    env("fm" -> Source, "compress" -> Effect) ~>
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
    env("fm" -> Source, "compress" -> Effect) ~>
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
    env("fm" -> Source, "compress" -> Effect) ~>
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
