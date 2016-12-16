package cspp

import org.scalatest.compatible.Assertion
import org.scalatest.FunSuite
import org.scalatest.Matchers

import absyn._
import AbsynSugar._
import csound.CsoundMinifier
import CsppDagNodes._

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
      case e: CsppCompileError => fail(e.toString + "\n" + e.getStackTrace.mkString("\n"))
    }
  }

  def testGoodProgram(input: Seq[StmtNode], expected: CsLines) = {
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

  implicit class ProgramTester(input: Seq[StmtNode]) extends Tester {
    override def run(output: CsLines) = testGoodProgram(input, output)
  }

  implicit class StmtTester(input: StmtNode) extends Tester {
    override def run(output: CsLines) =
    testGoodInput[StmtNode](
        input, CsppTranslator.translateStmtNode(CsppTranslator.EmptyContext, _)._2, output)
  }

  implicit class ExprTester(input: Expr) extends Tester {
    override def run(output: CsLines) =
      testGoodInput[Expr](
        input, CsppTranslator.translateExpr(CsppTranslator.EmptyContext, _)._2, output)
  }

  // Syntactic sugar for AST
  def astVar(id: String, ty: CsppType) = Application(Ident(id), Seq()) annotated ty
  def astNum(n: Double) = Num(n) annotated Number
  def astBinOp(l: Expr, op: Bop, r: Expr) = BinOp(l, op, r) annotated Number

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Expression tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  test("expression.number") {
    astNum(1) ~> ""
  }

  test("expression.var.number") {
    astVar("foo", Number) ~> "i0 cspp_foo"
  }

  test("expression.var.component") {
    CompNode(Seq(), Seq("a0"), astVar("foo", Source)) ~> "a0 cspp_foo"
  }

  test("expression.function") {
    (Application(Ident("foo"), Seq(
      astNum(1),
      astVar("arg", Number))
    ) annotated Number) ~>
    Seq(
        "i0 cspp_arg",
        "i1 cspp_foo 1.0, i0"
      )
  }

  test("expression.function.functionArgs") {
    (Application(Ident("foo"), Seq(
      Application(Ident("bar"), Seq(
        astNum(1))
      ) annotated Number)
    ) annotated Number) ~>
    Seq (
      "i0 cspp_bar 1.0",
      "i1 cspp_foo i0"
    )
  }

  test("expression.source") {
    CompNode(Seq(), Seq("a0"), Application(Ident("source"), Seq(
      astNum(1),
      astVar("arg", Number))
    ) annotated Source) ~>
    Seq(
      "i0 cspp_arg",
      "a0 cspp_source 1.0, i0"
    )
  }

  test("expression.source.functionArgs") {
    CompNode(Seq(), Seq("a0"), Application(Ident("source"), Seq(
      Application(Ident("arg"), Seq(
        astNum(1))
      ) annotated Number)
    ) annotated Source) ~>
    Seq (
      "i0 cspp_arg 1.0",
      "a0 cspp_source i0"
    )
  }

  test("expression.effect") {
    CompNode(Seq("a0"), Seq("a1"), Application(Ident("effect"), Seq(
      astNum(1),
      astVar("arg", Number))
    ) annotated Effect) ~>
    Seq(
      "i0 cspp_arg",
      "a1 cspp_effect a0, 1.0, i0"
    )
  }

  test("expression.effect.functionArgs") {
    CompNode(Seq("a0"), Seq("a1"), Application(Ident("effect"), Seq(
      Application(Ident("arg"), Seq(
        astNum(1))
      ) annotated Number)
    ) annotated Effect) ~>
    Seq (
      "i0 cspp_arg 1.0",
      "a1 cspp_effect a0, i0"
    )
  }

  test("expression.mux") {
    CompNode(Seq("a0", "a1"), Seq("a2"), astVar("mux", Component(2, 1))) ~> "a2 cspp_mux a0, a1"
  }

   test("expression.demux") {
    CompNode(Seq("a0"), Seq("a1", "a2"), astVar("demux", Component(1, 2))) ~> "a1, a2 cspp_demux a0"
  }

  test("expression.chain.source.one") {
    CompNode(Seq(), Seq("a0"), Chain(Seq(
      CompNode(Seq(), Seq("a0"), astVar("source", Source))
    )) annotated Source) ~>
    "a0 cspp_source"
  }

  test("expression.chain.source.withEffect") {
    CompNode(Seq(), Seq("a1"), Chain(Seq(
      CompNode(Seq(), Seq("a0"), astVar("source", Source)),
      CompNode(Seq("a0"), Seq("a1"), astVar("effect", Effect))
    )) annotated Source) ~>
    Seq(
      "a0 cspp_source",
      "a1 cspp_effect a0"
    )
  }

  test("expression.chain.effect.empty") {
    CompNode(Seq(), Seq(), Chain(Seq()) annotated Effect) ~> ""
  }

  test("expression.chain.effect.one") {
    CompNode(Seq("a0"), Seq("a1"), Chain(Seq(
      CompNode(Seq("a0"), Seq("a1"), astVar("effect", Effect))
    )) annotated Effect) ~> "a1 cspp_effect a0"
  }

  test("expression.chain.effect.many") {
    CompNode(Seq("a0"), Seq("a2"), Chain(Seq(
      CompNode(Seq("a0"), Seq("a1"), astVar("effect1", Effect)),
      CompNode(Seq("a1"), Seq("a2"), astVar("effect2", Effect))
    )) annotated Effect) ~>
    Seq(
      "a1 cspp_effect1 a0",
      "a2 cspp_effect2 a1"
    )
  }

  test("expression.parallel.moreIns") {
    // parallel { effect mux }
    CompNode(Seq("a0", "a1", "a2"), Seq("a3", "a4"), Parallel(Seq(
      CompNode(Seq("a0"), Seq("a3"), astVar("effect", Effect)),
      CompNode(Seq("a1", "a2"), Seq("a4"), astVar("mux", Component(2, 1)))
    )) annotated Component(3, 2)) ~>
    Seq(
      "a3 cspp_effect a0",
      "a4 cspp_mux a1, a2"
    )
  }

  test("expression.parallel.moreOuts") {
    // parallel { source effect }
    CompNode(Seq("a0"), Seq("a1", "a2"), Parallel(Seq(
      CompNode(Seq(), Seq("a1"), astVar("source", Source)),
      CompNode(Seq("a0"), Seq("a2"), astVar("effect", Effect))
    )) annotated Component(1, 2)) ~>
    Seq(
      "a1 cspp_source",
      "a2 cspp_effect a0"
    )
  }

  test("expression.parallel.equalInOut") {
    // parallel { effect1 effect2 }
    CompNode(Seq("a0", "a1", "a2"), Seq("a3", "a4", "a5"), Parallel(Seq(
        CompNode(Seq("a0"), Seq("a3"), astVar("effect1", Effect)),
        CompNode(Seq("a1", "a2"), Seq("a4", "a5"), astVar("effect2", Component(2, 2)))
    )) annotated Component(3, 3)) ~>
    Seq(
      "a3 cspp_effect1 a0",
      "a4, a5 cspp_effect2 a1, a2"
    )
  }

  def testBinOp(op: Bop, translatedOp: String) = {
    astBinOp(astNum(1), op, astNum(2)) ~> Seq(s"i0 = 1.0 $translatedOp 2.0")
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
      "i0 cspp_foo 2.0",
      "i1 = 1.0 + i0"
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
      "i0 = 1.0 + 2.0",
      "i1 = 3.0 - 4.0",
      "i2 = i0 * i1"
    )
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Statement tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  def scalarAssignment(id: Ident, params: Seq[Ident], body: Expr) =
    AssignNode(Seq(), Seq(), Assignment(id, params, body))

  test("statement.assignment.number") {
    scalarAssignment(Ident("foo"), Seq(), astNum(1)) ~> Seq(
      "opcode cspp_foo, i, 0",
      "i0 = 1.0",
      "xout i0",
      "endop"
    )
  }

  test("statement.assignment.source") {
    AssignNode(
      Seq(), Seq("a0"), Assignment(Ident("foo"), Seq(),
        CompNode(Seq(), Seq("a0"), astVar("fm", Source)))) ~>
    Seq(
      "opcode cspp_foo, a, 0",
      "a0 cspp_fm",
      "xout a0",
      "endop"
    )
  }

  test("statement.assignment.effect") {
    AssignNode(
      Seq("a0"), Seq("a1"), Assignment(Ident("foo"), Seq(),
        CompNode(Seq("a0"), Seq("a1"), astVar("compress", Effect)))) ~>
    Seq(
      "opcode cspp_foo, a, a",
      "a0 xin",
      "a1 cspp_compress a0",
      "xout a1",
      "endop"
    )
  }

  test("statement.assignment.function.number") {
    scalarAssignment(Ident("foo"), Seq(Ident("arg")), astVar("arg", Number)) ~> Seq(
      "opcode cspp_foo, i, i",
      "iarg xin",
      "xout iarg",
      "endop"
    )
  }

  test("statement.assignment.function.source") {
    AssignNode(Seq(), Seq("a0"),
      Assignment(Ident("foo"), Seq(Ident("arg")),
        CompNode(Seq(), Seq("a0"), Application(Ident("source"), Seq(astVar("arg", Number))) annotated Source)
      )
    ) ~> Seq(
      "opcode cspp_foo, a, i",
      "iarg xin",
      "a0 cspp_source iarg",
      "xout a0",
      "endop"
    )
  }

  test("statement.assignment.function.effect") {
    AssignNode(Seq("a0"), Seq("a1"),
      Assignment(Ident("foo"), Seq(Ident("arg")),
        CompNode(Seq("a0"), Seq("a1"), Application(Ident("effect"), Seq(astVar("arg", Number))) annotated Effect)
      )
    ) ~> Seq(
      "opcode cspp_foo, a, ai",
      "a0, iarg xin",
      "a1 cspp_effect a0, iarg",
      "xout a1",
      "endop"
    )
  }

  def sourceInstr(instrId: String): CsLines = Seq(
    s"instr $instrId",
    "iamp ampmidi 1",
    "ifreq cpsmidi",
    "a0 cspp_source",
    s"chout a0, $instrId",
    "out a0",
    "endin"
  )

  def sourceInstr: CsLines = sourceInstr("1")

  test("statement.instrument.chain") {
    InstrNode(Seq(), Seq("a1"), Instrument(Seq(astNum(2)),
      CompNode(Seq(), Seq("a1"), Chain(Seq(
        CompNode(Seq(), Seq("a0"), astVar("source", Source)),
        CompNode(Seq("a0"), Seq("a1"), astVar("effect", Effect))
      )) annotated Source)
    )) ~> Seq(
      "instr 1",
      "iamp ampmidi 1",
      "ifreq cpsmidi",
      "a0 cspp_source",
      "a1 cspp_effect a0",
      "chout a1, 1",
      "out a1",
      "endin",
      "massign 2.0, 1"
    )
  }

  test("statement.instrument.numChannel") {
    InstrNode(Seq(), Seq("a0"), Instrument(Seq(astNum(2)),
      CompNode(Seq(), Seq("a0"), astVar("source", Source)))) ~>
    (sourceInstr ++ Seq(
      "massign 2.0, 1"
    ))
  }

  test("statement.instrument.funcChannel") {
    InstrNode(Seq(), Seq("a0"), Instrument(
      Seq(
        Application(Ident("channel"), Seq(
          astNum(2))
        ) annotated Number
      ),
      CompNode(Seq(), Seq("a0"), astVar("source", Source))
    )) ~>
    (sourceInstr ++ Seq(
      "i0 cspp_channel 2.0",
      "massign i0, 1"
    ))
  }

  test("statement.instrument.manyChannels") {
    InstrNode(Seq(), Seq("a0"), Instrument(
      Seq(astNum(2), astNum(3)),
      CompNode(Seq(), Seq("a0"), astVar("source", Source))
    )) ~>
    (sourceInstr ++ Seq(
      "massign 2.0, 1",
      "massign 3.0, 1"
    ))
  }

  test("statement.instrument.midiVelocity") {
    InstrNode(Seq(), Seq("a0"), Instrument(
      Seq(astNum(2)),
      CompNode(Seq(), Seq("a0"), Application(
        Ident("source"), Seq(astVar("amp", Number))
      ) annotated Source)
    )) ~>
    Seq(
      "instr 1",
      "iamp ampmidi 1",
      "ifreq cpsmidi",
      "a0 cspp_source iamp",
      "chout a0, 1",
      "out a0",
      "endin",

      "massign 2.0, 1"
    )
  }

  test("statement.instrument.midiFreq") {
    InstrNode(Seq(), Seq("a0"), Instrument(
      Seq(astNum(2)),
      CompNode(Seq(), Seq("a0"), Application(Ident("source"), Seq(
        astVar("freq", Number))
      ) annotated Source)
    )) ~>
    Seq(
      "instr 1",
      "iamp ampmidi 1",
      "ifreq cpsmidi",
      "a0 cspp_source ifreq",
      "chout a0, 1",
      "out a0",
      "endin",

      "massign 2.0, 1"
    )
  }

  test("statement.instrument.manyInstruments") {
    Seq(
      InstrNode(Seq(), Seq("a0"), Instrument(
        Seq(astNum(3)),
        CompNode(Seq(), Seq("a0"), astVar("source", Source))
      )),
      InstrNode(Seq(), Seq("a0"), Instrument(
        Seq(astNum(4)),
        CompNode(Seq(), Seq("a0"), astVar("source", Source))
      ))
    ) ~>
    (sourceInstr ++ Seq(
      "massign 3.0, 1"
    ) ++
    sourceInstr("2") ++ Seq(
      "massign 4.0, 2"
    ))
  }

  test("statement.sends.chain") {
    SendsNode(Seq("a0"), Seq("a2"), Sends(astNum(2),
      CompNode(Seq("a0"), Seq("a2"), Chain(Seq(
        CompNode(Seq("a0"), Seq("a1"), astVar("effect1", Effect)),
        CompNode(Seq("a1"), Seq("a2"), astVar("effect2", Effect))
      )) annotated Effect)
    )) ~>  Seq(
      s"instr 1",
      s"a0 channel 2.0",
      "a1 cspp_effect1 a0",
      "a2 cspp_effect2 a1",
      "out a2",
      "endin",
      "turnon 1"
    )
  }

  test("statement.sends.numChannel") {
    SendsNode(Seq("a0"), Seq("a1"), Sends(astNum(2),
      CompNode(Seq("a0"), Seq("a1"), astVar("effect", Effect)))) ~>
    Seq(
      "instr 1",
      "a0 channel 2.0",
      "a1 cspp_effect a0",
      "out a1",
      "endin",
      "turnon 1"
    )
  }

  test("statement.sends.funcChannel") {
    SendsNode(Seq("a0"), Seq("a1"), Sends(
        Application(Ident("channel"), Seq(
          astNum(2))
        ) annotated Number,
      CompNode(Seq("a0"), Seq("a1"), astVar("effect", Effect))
    )) ~>
    Seq(
      "instr 1",
      "i0 cspp_channel 2.0",
      "a0 channel i0",
      "a1 cspp_effect a0",
      "out a1",
      "endin",
      "turnon 1"
    )
  }

}
