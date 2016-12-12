package cspp

import scala.language.postfixOps
import org.scalatest.FunSuite
import org.scalatest.Matchers

import absyn._
import AbsynSugar._
import CsppDagNodes._

class DagSuite extends FunSuite with Matchers {

  implicit class SeqTester(input: Seq[Statement]) {
    def ~>(expected: Seq[Node]) = CsppDag(input) match {
      case Right(output) => output should be (expected)
      case Left(err) => fail(s"Translation failed: $err")
    }
  }

  implicit class StmtTester(input: Statement) {
    def ~>(expected: Node) = Seq(input) ~> Seq(expected)
  }

  def astVar(name: String, ty: CsppType) = Application(name, Seq()) annotated ty
  def astNum(n: Int) = Num(n) annotated Number

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Number assignment tests: numbers should not be transformed
  //////////////////////////////////////////////////////////////////////////////////////////////////

  test("assignment.number.literal") {
    val stmt = Assignment("foo", Seq(), astNum(1))
    stmt ~> AssignNode(Seq(), Seq(), stmt)
  }

  test("assignment.number.application") {
    val stmt = Assignment("foo", Seq(), astVar("bar", Number))
    stmt ~> AssignNode(Seq(), Seq(), stmt)
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Single component assignment tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  def testAppComponentWithArity(inArity: Int, outArity: Int) = {
    val ids = 0 until (inArity + outArity) toList
    val inputs = ids.take(inArity).map("a" + _)
    val outputs = ids.drop(inArity).map("a" + _)

    Assignment("foo", Seq(), astVar("bar", Component(inArity, outArity))) ~>
      AssignNode(
        inputs, outputs, Assignment("foo", Seq(), CompNode(
          inputs, outputs, astVar("bar", Component(inArity, outArity))
        ))
      )
  }

  test("assignment.application.source") {
    testAppComponentWithArity(0, 1)
  }

  test("assignment.application.effect") {
    testAppComponentWithArity(1, 1)
  }

  test("assignment.application.mux") {
    testAppComponentWithArity(3, 1)
  }

  test("assignment.application.demux") {
    testAppComponentWithArity(1, 3)
  }

  test("assignment.application.partialMux") {
    testAppComponentWithArity(4, 2)
  }

  test("assignment.application.partialDemux") {
    testAppComponentWithArity(2, 4)
  }

  test("assignment.application.parallel") {
    testAppComponentWithArity(4, 4)
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Chain assignment tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  test("assignment.chain.flat.source") {
    // foo = {source effect1 effect2}
    Assignment("foo", Seq(), Chain(Seq(
      astVar("source", Source),
      astVar("effect1", Effect),
      astVar("effect2", Effect)
    )) annotated Source) ~>
    AssignNode(Seq(), Seq("a2"), Assignment("foo", Seq(), CompNode(Seq(), Seq("a2"), Chain(Seq(
      CompNode(Seq(), Seq("a0"), astVar("source", Source)),
      CompNode(Seq("a0"), Seq("a1"), astVar("effect1", Effect)),
      CompNode(Seq("a1"), Seq("a2"), astVar("effect2", Effect))
    )) annotated Source)))
  }

  test("assignment.chain.flat.effect") {
    // foo = {effect1 effect2}
    Assignment("foo", Seq(), Chain(Seq(
      astVar("effect1", Effect),
      astVar("effect2", Effect)
    )) annotated Effect) ~>
    AssignNode(Seq("a0"), Seq("a2"), Assignment("foo", Seq(), CompNode(Seq("a0"), Seq("a2"), Chain(Seq(
      CompNode(Seq("a0"), Seq("a1"), astVar("effect1", Effect)),
      CompNode(Seq("a1"), Seq("a2"), astVar("effect2", Effect))
    )) annotated Effect)))
  }

  test("assignment.chain.flat.mux") {
    // foo = {mux effect}
    Assignment("foo", Seq(), Chain(Seq(
      astVar("mux", Component(2, 1)),
      astVar("effect", Effect)
    )) annotated Component(2, 1)) ~>
    AssignNode(Seq("a0", "a1"), Seq("a3"), Assignment("foo", Seq(), CompNode(Seq("a0", "a1"), Seq("a3"), Chain(Seq(
      CompNode(Seq("a0", "a1"), Seq("a2"), astVar("mux", Component(2, 1))),
      CompNode(Seq("a2"), Seq("a3"), astVar("effect", Effect))
    )) annotated Component(2, 1))))
  }

  test("assignment.chain.flat.demux") {
    // foo = {effect demux}
    Assignment("foo", Seq(), Chain(Seq(
      astVar("effect", Effect),
      astVar("demux", Component(1, 2))
    )) annotated Component(1, 2)) ~>
    AssignNode(Seq("a0"), Seq("a2", "a3"), Assignment("foo", Seq(), CompNode(Seq("a0"), Seq("a2", "a3"), Chain(Seq(
      CompNode(Seq("a0"), Seq("a1"), astVar("effect", Effect)),
      CompNode(Seq("a1"), Seq("a2", "a3"), astVar("demux", Component(1, 2)))
    )) annotated Component(1, 2))))
  }

  test("assignment.chain.flat.partialMux") {
    // foo = {mux effect}
    Assignment("foo", Seq(), Chain(Seq(
      astVar("mux", Component(3, 2)),
      astVar("effect", Component(2, 2))
    )) annotated Component(3, 2)) ~>
    AssignNode(Seq("a0", "a1", "a2"), Seq("a5", "a6"), Assignment(
      "foo", Seq(), CompNode(Seq("a0", "a1", "a2"), Seq("a5", "a6"), Chain(Seq(
        CompNode(Seq("a0", "a1", "a2"), Seq("a3", "a4"), astVar("mux", Component(3, 2))),
        CompNode(Seq("a3", "a4"), Seq("a5", "a6"), astVar("effect", Component(2, 2)))
      )) annotated Component(3, 2)
    )))
  }

  test("assignment.chain.flat.partialDemux") {
    // foo = {effect demux}
    Assignment("foo", Seq(), Chain(Seq(
      astVar("effect", Component(2, 2)),
      astVar("demux", Component(2, 3))
    )) annotated Component(2, 3)) ~>
    AssignNode(Seq("a0", "a1"), Seq("a4", "a5", "a6"), Assignment(
      "foo", Seq(), CompNode(Seq("a0", "a1"), Seq("a4", "a5", "a6"), Chain(Seq(
        CompNode(Seq("a0", "a1"), Seq("a2", "a3"), astVar("effect", Component(2, 2))),
        CompNode(Seq("a2", "a3"), Seq("a4", "a5", "a6"), astVar("demux", Component(2, 3)))
      )) annotated Component(2, 3)
    )))
  }

  test("assignment.chain.flat.parallel") {
    // foo = {effect1 effect2}
    Assignment("foo", Seq(), Chain(Seq(
      astVar("effect1", Component(2, 2)),
      astVar("effect2", Component(2, 2))
    )) annotated Component(2, 2)) ~>
    AssignNode(Seq("a0", "a1"), Seq("a4", "a5"), Assignment(
      "foo", Seq(), CompNode(Seq("a0", "a1"), Seq("a4", "a5"), Chain(Seq(
        CompNode(Seq("a0", "a1"), Seq("a2", "a3"), astVar("effect1", Component(2, 2))),
        CompNode(Seq("a2", "a3"), Seq("a4", "a5"), astVar("effect2", Component(2, 2)))
      )) annotated Component(2, 2)
    )))
  }

  test("assignment.chain.nested.parallel") {
    // foo = { parallel {effect source} mux }
    Assignment("foo", Seq(), Chain(Seq(
      Parallel(Seq(
        astVar("effect", Effect),
        astVar("source", Source)
      )) annotated Component(1, 2),
      astVar("mux", Component(2, 1))
    )) annotated Effect) ~>
    AssignNode(Seq("a0"), Seq("a3"), Assignment(
      "foo", Seq(), CompNode(Seq("a0"), Seq("a3"), Chain(Seq(
        CompNode(Seq("a0"), Seq("a1", "a2"), Parallel(Seq(
          CompNode(Seq("a0"), Seq("a1"), astVar("effect", Effect)),
          CompNode(Seq(), Seq("a2"), astVar("source", Source))
        )) annotated Component(1, 2)),
        CompNode(Seq("a1", "a2"), Seq("a3"), astVar("mux", Component(2, 1)))
      )) annotated Effect
    )))
  }

  test("assignment.chain.nested.chain") {
    // foo = { {source effect1} effect2 }
    Assignment("foo", Seq(), Chain(Seq(
      Chain(Seq(
        astVar("source", Source),
        astVar("effect1", Effect)
      )) annotated Source,
      astVar("effect2", Effect)
    )) annotated Source) ~>
    AssignNode(Seq(), Seq("a2"), Assignment(
      "foo", Seq(), CompNode(Seq(), Seq("a2"), Chain(Seq(
        CompNode(Seq(), Seq("a1"), Chain(Seq(
          CompNode(Seq(), Seq("a0"), astVar("source", Source)),
          CompNode(Seq("a0"), Seq("a1"), astVar("effect1", Effect))
        )) annotated Source),
        CompNode(Seq("a1"), Seq("a2"), astVar("effect2", Effect))
      )) annotated Source
    )))
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Parallel assignment tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  test("assignment.parallel.flat.moreIns") {
    // foo = parallel { effect mux }
    Assignment("foo", Seq(), Parallel(Seq(
      astVar("effect", Effect),
      astVar("mux", Component(2, 1))
    )) annotated Component(3, 2)) ~>
    AssignNode(Seq("a0", "a1", "a2"), Seq("a3", "a4"), Assignment(
      "foo", Seq(), CompNode(Seq("a0", "a1", "a2"), Seq("a3", "a4"), Parallel(Seq(
        CompNode(Seq("a0"), Seq("a3"), astVar("effect", Effect)),
        CompNode(Seq("a1", "a2"), Seq("a4"), astVar("mux", Component(2, 1)))
      )) annotated Component(3, 2))
    ))
  }

  test("assignment.parallel.flat.moreOuts") {
    // foo = parallel { source effect }
    Assignment("foo", Seq(), Parallel(Seq(
      astVar("source", Source),
      astVar("effect", Effect)
    )) annotated Component(1, 2)) ~>
    AssignNode(Seq("a0"), Seq("a1", "a2"), Assignment(
      "foo", Seq(), CompNode(Seq("a0"), Seq("a1", "a2"), Parallel(Seq(
        CompNode(Seq(), Seq("a1"), astVar("source", Source)),
        CompNode(Seq("a0"), Seq("a2"), astVar("effect", Effect))
      )) annotated Component(1, 2))
    ))
  }

  test("assignment.parallel.flat.equalInOut") {
    // foo = parallel { effect1 effect2 }
    Assignment("foo", Seq(), Parallel(Seq(
      astVar("effect1", Effect),
      astVar("effect2", Component(2, 2))
    )) annotated Component(3, 3)) ~>
    AssignNode(Seq("a0", "a1", "a2"), Seq("a3", "a4", "a5"), Assignment(
      "foo", Seq(), CompNode(Seq("a0", "a1", "a2"), Seq("a3", "a4", "a5"), Parallel(Seq(
        CompNode(Seq("a0"), Seq("a3"), astVar("effect1", Effect)),
        CompNode(Seq("a1", "a2"), Seq("a4", "a5"), astVar("effect2", Component(2, 2)))
      )) annotated Component(3, 3)
    )))
  }

  test("assignment.parallel.nested.parallel") {
    // foo = parallel { source1 parallel { source2 effect } }
    Assignment("foo", Seq(), Parallel(Seq(
      astVar("source1", Source),
      Parallel(Seq(
        astVar("source2", Source),
        astVar("effect", Effect)
      )) annotated Component(1, 2)
    )) annotated Component(1, 3)) ~>
    AssignNode(Seq("a0"), Seq("a1", "a2", "a3"), Assignment(
      "foo", Seq(), CompNode(Seq("a0"), Seq("a1", "a2", "a3"), Parallel(Seq(
        CompNode(Seq(), Seq("a1"), astVar("source1", Source)),
        CompNode(Seq("a0"), Seq("a2", "a3"), Parallel(Seq(
          CompNode(Seq(), Seq("a2"), astVar("source2", Source)),
          CompNode(Seq("a0"), Seq("a3"), astVar("effect", Effect))
        )) annotated Component(1, 2))
      )) annotated Component(1, 3))
    ))
  }

  test("assignment.parallel.nested.chain") {
    // foo = parallel { { source1 effect } source2 }
    Assignment("foo", Seq(), Parallel(Seq(
      Chain(Seq(
        astVar("source1", Source),
        astVar("effect", Effect)
      )) annotated Source,
      astVar("source2", Source)
    )) annotated Component(0, 2)) ~>
    AssignNode(Seq(), Seq("a1", "a2"), Assignment(
      "foo", Seq(), CompNode(Seq(), Seq("a1", "a2"), Parallel(Seq(
        CompNode(Seq(), Seq("a1"), Chain(Seq(
          CompNode(Seq(), Seq("a0"), astVar("source1", Source)),
          CompNode(Seq("a0"), Seq("a1"), astVar("effect", Effect))
        )) annotated Source),
        CompNode(Seq(), Seq("a2"), astVar("source2", Source))
      )) annotated Component(0, 2))
    ))
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Single component instrument tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  test("instrument.application") {
    Instrument(Seq(astNum(1)), astVar("foo", Component(0, 1))) ~>
      InstrNode(
        Seq(), Seq("a0"), Instrument(Seq(astNum(1)), CompNode(
          Seq(), Seq("a0"), astVar("foo", Component(0, 1))
        ))
      )
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Chain instrument tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  test("instrument.chain.flat.source") {
    // instr(1) = {source effect1 effect2}
    Instrument(Seq(astNum(1)), Chain(Seq(
      astVar("source", Source),
      astVar("effect1", Effect),
      astVar("effect2", Effect)
    )) annotated Source) ~>
    InstrNode(Seq(), Seq("a2"), Instrument(Seq(astNum(1)), CompNode(Seq(), Seq("a2"), Chain(Seq(
      CompNode(Seq(), Seq("a0"), astVar("source", Source)),
      CompNode(Seq("a0"), Seq("a1"), astVar("effect1", Effect)),
      CompNode(Seq("a1"), Seq("a2"), astVar("effect2", Effect))
    )) annotated Source)))
  }

  test("instrument.chain.nested.parallel") {
    // instr(1) = { parallel {source1 source2} mux }
    Instrument(Seq(astNum(1)), Chain(Seq(
      Parallel(Seq(
        astVar("source1", Source),
        astVar("source2", Source)
      )) annotated Component(0, 2),
      astVar("mux", Component(2, 1))
    )) annotated Source) ~>
    InstrNode(Seq(), Seq("a2"), Instrument(
      Seq(astNum(1)), CompNode(Seq(), Seq("a2"), Chain(Seq(
        CompNode(Seq(), Seq("a0", "a1"), Parallel(Seq(
          CompNode(Seq(), Seq("a0"), astVar("source1", Source)),
          CompNode(Seq(), Seq("a1"), astVar("source2", Source))
        )) annotated Component(0, 2)),
        CompNode(Seq("a0", "a1"), Seq("a2"), astVar("mux", Component(2, 1)))
      )) annotated Source
    )))
  }

  test("instrument.chain.nested.chain") {
    // instr(1) = { {source effect1} effect2 }
    Instrument(Seq(astNum(1)), Chain(Seq(
      Chain(Seq(
        astVar("source", Source),
        astVar("effect1", Effect)
      )) annotated Source,
      astVar("effect2", Effect)
    )) annotated Source) ~>
    InstrNode(Seq(), Seq("a2"), Instrument(
      Seq(astNum(1)), CompNode(Seq(), Seq("a2"), Chain(Seq(
        CompNode(Seq(), Seq("a1"), Chain(Seq(
          CompNode(Seq(), Seq("a0"), astVar("source", Source)),
          CompNode(Seq("a0"), Seq("a1"), astVar("effect1", Effect))
        )) annotated Source),
        CompNode(Seq("a1"), Seq("a2"), astVar("effect2", Effect))
      )) annotated Source
    )))
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Sends tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  test("sends.chain.flat.source") {
    // sends(1) = {effect1 effect2}
    Sends(astNum(1), Chain(Seq(
      astVar("effect1", Effect),
      astVar("effect2", Effect)
    )) annotated Effect) ~>
    SendsNode(Seq("a0"), Seq("a2"), Sends(astNum(1), CompNode(Seq("a0"), Seq("a2"), Chain(Seq(
      CompNode(Seq("a0"), Seq("a1"), astVar("effect1", Effect)),
      CompNode(Seq("a1"), Seq("a2"), astVar("effect2", Effect))
    )) annotated Effect)))
  }

  test("sends.chain.nested.parallel") {
    // instr(1) = { parallel {source effect} mux }
    Sends(astNum(1), Chain(Seq(
      Parallel(Seq(
        astVar("source", Source),
        astVar("effect", Effect)
      )) annotated Component(1, 2),
      astVar("mux", Component(2, 1))
    )) annotated Effect) ~>
    SendsNode(Seq("a0"), Seq("a3"), Sends(
      astNum(1), CompNode(Seq("a0"), Seq("a3"), Chain(Seq(
        CompNode(Seq("a0"), Seq("a1", "a2"), Parallel(Seq(
          CompNode(Seq(), Seq("a1"), astVar("source", Source)),
          CompNode(Seq("a0"), Seq("a2"), astVar("effect", Effect))
        )) annotated Component(1, 2)),
        CompNode(Seq("a1", "a2"), Seq("a3"), astVar("mux", Component(2, 1)))
      )) annotated Effect
    )))
  }

  test("sends.chain.nested.chain") {
    // instr(1) = { { effect1 } effect2 }
    Sends(astNum(1), Chain(Seq(
      Chain(Seq(
        astVar("effect1", Effect)
      )) annotated Effect,
      astVar("effect2", Effect)
    )) annotated Effect) ~>
    SendsNode(Seq("a0"), Seq("a2"), Sends(
      astNum(1), CompNode(Seq("a0"), Seq("a2"), Chain(Seq(
        CompNode(Seq("a0"), Seq("a1"), Chain(Seq(
          CompNode(Seq("a0"), Seq("a1"), astVar("effect1", Effect))
        )) annotated Effect),
        CompNode(Seq("a1"), Seq("a2"), astVar("effect2", Effect))
      )) annotated Effect
    )))
  }

}
