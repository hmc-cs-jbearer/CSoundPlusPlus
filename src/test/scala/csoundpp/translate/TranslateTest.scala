package cspp

import org.scalatest.compatible.Assertion
import org.scalatest.FunSuite
import org.scalatest.Matchers

import absyn._
import csound.CsoundMinifier

class TranslateSuite extends FunSuite with Matchers {

  type CsLine = CsppTranslator.CsLine
  type CsLines = CsppTranslator.CsLines

  // Strip out all of the stuff we don't care about when testing for equality, like blank lines
  def process(input: CsLines): CsLines = CsoundMinifier(input) match {
    case Right(output) => output
    case Left(err) => throw new CsppCompileError(
      NoLocation, s"Parse error in CSound code:\n${input.mkString("\n")}\n$err")
  }

  def testGoodInput[T <: ASTElem](input: T, translator: T => CsLines, output: CsLines) = {
    try {
      process(translator(input)) should equal (process(output))
    } catch {
      case err: CsppCompileError => fail(s"Translation failed: $err")
    }
  }

  def testGoodProgram(input: Seq[Statement], expected: CsLines) = {
    CsppTranslator(input) match {
      case Right(output) => process(output) should equal (process(expected))
      case Left(err) => fail(s"Translation failed: $err")
    }
  }

  abstract class Tester {
    def run(output: CsLines): Assertion

    def ~>(output: CsLines): Assertion = run(output)
    def ~>(output: CsLine): Assertion = run(Seq(output))
  }

  implicit class ProgramTester(input: Seq[Statement]) extends Tester {
    override def run(output: CsLines) = testGoodProgram(input, output)
  }

  implicit class StmtTester(input: Statement) extends Tester {
    override def run(output: CsLines) =
      testGoodInput[Statement](input, CsppTranslator.translateStmt(emptyContext, _)._2, output)
  }

  implicit class ExprTester(input: Expr) extends Tester {
    override def run(output: CsLines) =
      testGoodInput[Expr](input, CsppTranslator.translateExpr(emptyContext, _)._2, output)
  }

  // Syntactic sugar for AST
  def astVar(id: String, ty: CsppType) = Application(Ident(id), Seq()) annotated ty
  def astNum(n: Double) = Num(n) annotated Number
  def astBinOp(l: Expr, op: Bop, r: Expr) = BinOp(l, op, r) annotated Number

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Expression tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  test("expression.number") {
    astNum(1) ~> "i0 = 1.0"
  }

  test("expression.var.number") {
    astVar("foo", Number) ~> "i0 cspp_foo"
  }

  test("expression.var.component") {
    astVar("foo", Source) ~> "asig cspp_foo"
  }

  test("expression.function") {
    (Application(Ident("foo"), Seq(
      astNum(1),
      astVar("arg", Number))
    ) annotated Number) ~>
    Seq(
        "i0 = 1.0",
        "i1 cspp_arg",
        "i2 cspp_foo i0, i1"
      )
  }

  test("expression.function.functionArgs") {
    (Application(Ident("foo"), Seq(
      Application(Ident("bar"), Seq(
        astNum(1))
      ) annotated Number)
    ) annotated Number) ~>
    Seq (
      "i0 = 1.0",
      "i1 cspp_bar i0",
      "i2 cspp_foo i1"
    )
  }

  test("expression.source") {
    (Application(Ident("source"), Seq(
      astNum(1),
      astVar("arg", Number))
    ) annotated Source) ~>
    Seq(
      "i0 = 1.0",
      "i1 cspp_arg",
      "asig cspp_source i0, i1"
    )
  }

  test("expression.source.functionArgs") {
    (Application(Ident("source"), Seq(
      Application(Ident("arg"), Seq(
        astNum(1))
      ) annotated Number)
    ) annotated Source) ~>
    Seq (
      "i0 = 1.0",
      "i1 cspp_arg i0",
      "asig cspp_source i1"
    )
  }

  test("expression.effect") {
    (Application(Ident("effect"), Seq(
      astNum(1),
      astVar("arg", Number))
    ) annotated Effect) ~>
    Seq(
      "i0 = 1.0",
      "i1 cspp_arg",
      "asig cspp_effect asig, i0, i1"
    )
  }

  test("expression.effect.functionArgs") {
    (Application(Ident("effect"), Seq(
      Application(Ident("arg"), Seq(
        astNum(1))
      ) annotated Number)
    ) annotated Effect) ~>
    Seq (
      "i0 = 1.0",
      "i1 cspp_arg i0",
      "asig cspp_effect asig, i1"
    )
  }

  test("expression.chain.source.one") {
    (Chain(Seq(astVar("source", Source))) annotated Source) ~> "asig cspp_source"
  }

  test("expression.chain.source.withEffect") {
    (Chain(Seq(
      astVar("source", Source),
      astVar("effect", Effect))
    ) annotated Source) ~>
    Seq(
      "asig cspp_source",
      "asig cspp_effect asig"
    )
  }

  test("expression.chain.effect.empty") {
    // When an effect is used, the identifier asig must already exist and contain the signal.
    // Therefore, to pass the signal through unchanged, we don't have to do anything.
    (Chain(Seq()) annotated Effect) ~> ""
  }

  test("expression.chain.effect.one") {
    (Chain(Seq(astVar("effect", Effect))) annotated Effect) ~> "asig cspp_effect asig"
  }

  test("expression.chain.effect.many") {
    (Chain(Seq(
      astVar("effect1", Effect),
      astVar("effect2", Effect))
    ) annotated Effect) ~>
    Seq(
      "asig cspp_effect1 asig",
      "asig cspp_effect2 asig"
    )
  }

  def testBinOp(op: Bop, translatedOp: String) = {
    astBinOp(astNum(1), op, astNum(2)) ~> Seq("i0 = 1.0", "i1 = 2.0", s"i2 = i0 $translatedOp i1")
  }

  test("expression.arithmetic.binOp.plus") {
    testBinOp(Plus, "+")
  }

  test("expression.arithmetic.binOp.minus") {
    testBinOp(Minus, "-")
  }

  test("expression.arithmetic.binOp.times") {
    testBinOp(Times, "*")
  }

  test("expression.arithmetic.binOp.divide") {
    testBinOp(Divide, "/")
  }

  test("expression.arithmetic.binOp.functionArgs") {
    astBinOp(
      astNum(1),
      Plus,
      Application(Ident("foo"), Seq(
        astNum(2))
      ) annotated Number
    ) ~>
    Seq(
      "i0 = 1.0",
      "i1 = 2.0",
      "i2 cspp_foo i1",
      "i3 = i0 + i2"
    )
  }

  test("expression.arithmetic.binOp.nested") {
    astBinOp(
      astBinOp(
        astNum(1),
        Plus,
        astNum(2)
      ),
      Times,
      astBinOp(
        astNum(3),
        Minus,
        astNum(4)
      )
    ) ~>
    Seq(
      "i0 = 1.0",
      "i1 = 2.0",
      "i2 = i0 + i1",

      "i3 = 3.0",
      "i4 = 4.0",
      "i5 = i3 - i4",

      "i6 = i2 * i5"
    )
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Statement tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  test("statement.assignment.number") {
    Assignment(Ident("foo"), Seq(), astNum(1)) ~> Seq(
      "opcode cspp_foo, i, 0",
      "i0 = 1.0",
      "xout i0",
      "endop"
    )
  }

  test("statement.assignment.source") {
    Assignment(Ident("foo"), Seq(), astVar("fm", Source)) ~> Seq(
      "opcode cspp_foo, a, 0",
      "asig cspp_fm",
      "xout asig",
      "endop"
    )
  }

  test("statement.assignment.effect") {
    Assignment(Ident("foo"), Seq(), astVar("compress", Effect)) ~> Seq(
      "opcode cspp_foo, a, a",
      "asig xin",
      "asig cspp_compress asig",
      "xout asig",
      "endop"
    )
  }

  test("statement.assignment.function.number") {
    Assignment(Ident("foo"), Seq(Ident("arg")), astVar("arg", Number)) ~> Seq(
      "opcode cspp_foo, i, i",
      "iarg xin",
      "i0 = iarg",
      "xout i0",
      "endop"
    )
  }

  test("statement.assignment.function.source") {
    Assignment(Ident("foo"), Seq(Ident("arg")), Application(Ident("source"), Seq(
      astVar("arg", Number))
    ) annotated Source) ~> Seq(
      "opcode cspp_foo, a, i",
      "iarg xin",
      "i0 = iarg",
      "asig cspp_source i0",
      "xout asig",
      "endop"
    )
  }

  test("statement.assignment.function.effect") {
    Assignment(Ident("foo"), Seq(Ident("arg")), Application(Ident("effect"), Seq(
      astVar("arg", Number))
    ) annotated Effect) ~> Seq(
      "opcode cspp_foo, a, ai",
      "asig, iarg xin",
      "i0 = iarg",
      "asig cspp_effect asig, i0",
      "xout asig",
      "endop"
    )
  }

  def sourceInstr(instrId: String): CsLines = Seq(
    s"instr $instrId",
    "iamp ampmidi 1",
    "ifreq cpsmidi",
    "asig cspp_source",
    "out asig",
    "endin"
  )

  def sourceInstr: CsLines = sourceInstr("1")

  test("statement.instrument.numChannel") {
    Instrument(Seq(astNum(2)), astVar("source", Source)) ~>
    (sourceInstr ++ Seq(
      "i0 = 2.0",
      "massign i0, 1"
    ))
  }

  test("statement.instrument.funcChannel") {
    Instrument(
      Seq(
        Application(Ident("channel"), Seq(
          astNum(2))
        ) annotated Number
      ),
      astVar("source", Source)
    ) ~>
    (sourceInstr ++ Seq(
      "i0 = 2.0",
      "i1 cspp_channel i0",
      "massign i1, 1"
    ))
  }

  test("statement.instrument.manyChannels") {
    Instrument(Seq(astNum(2), astNum(3)), astVar("source", Source)) ~>
    (sourceInstr ++ Seq(
      "i0 = 2.0",
      "massign i0, 1",
      "i1 = 3.0",
      "massign i1, 1"
    ))
  }

  test("statement.instrument.midiVelocity") {
    Instrument(Seq(astNum(2)), Application(Ident("source"), Seq(
      astVar("amp", Number))
    ) annotated Source) ~>
    Seq(
      "instr 1",
      "iamp ampmidi 1",
      "ifreq cpsmidi",
      "i0 = iamp",
      "asig cspp_source i0",
      "out asig",
      "endin",

      "i0 = 2.0",
      "massign i0, 1"
    )
  }

  test("statement.instrument.midiFreq") {
    Instrument(Seq(astNum(2)), Application(Ident("source"), Seq(
      astVar("freq", Number))
    ) annotated Source) ~>
    Seq(
      "instr 1",
      "iamp ampmidi 1",
      "ifreq cpsmidi",
      "i0 = ifreq",
      "asig cspp_source i0",
      "out asig",
      "endin",

      "i0 = 2.0",
      "massign i0, 1"
    )
  }

  test("statement.instrument.manyInstruments") {
    Seq(
      Instrument(Seq(astNum(3)), astVar("source", Source)),
      Instrument(Seq(astNum(4)), astVar("source", Source))
    ) ~>
    (sourceInstr ++ Seq(
      "i0 = 3.0",
      "massign i0, 1"
    ) ++
    sourceInstr("2") ++ Seq(
      "i1 = 4.0",
      "massign i1, 2"
    ))
  }

}
