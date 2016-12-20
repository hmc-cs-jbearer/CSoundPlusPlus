package cspp

import org.scalatest.compatible.Assertion
import org.scalatest.FunSuite
import org.scalatest.Matchers

import absyn._
import AbsynSugar._

class LetResolverSuite extends FunSuite with Matchers {

  def test(input: Seq[Statement], expected: Seq[Statement]) = CsppLetResolver(input) match {
    case Right(output) => output should be (expected)
    case Left(err)     => fail("Error during let resolution: $err")
  }

  implicit class SeqTester(input: Seq[Statement]) {
    def ~>(output: Seq[Statement]): Assertion = test(input, output)
  }

  implicit class StmtTester(input: Statement) {
    def ~>(output: Statement): Assertion = Seq(input) ~> Seq(output)
    def ~>(output: Seq[Statement]): Assertion = Seq(input) ~> output
  }

  def astNum(n: Double) = Num(n) annotated Number

  def astVar(name: String, ty: CsppType) = Application(name, Seq()) annotated ty

  def astBinOp(l: Expr, op: Bop, r: Expr) =
    BinOp(l annotated Number, op, r annotated Number) annotated Number

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Assignment tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  test("statement.assignment.let.one") {
    Assignment("foo", Seq(), Let(Seq(
      Assignment("y", Seq(), astVar("x", Number))),
      astVar("y", Number)
    ) annotated Number) ~>
    Seq(
      Assignment("_let0y", Seq(), astVar("x", Number)),
      Assignment("foo", Seq(), astVar("_let0y", Number))
    )
  }

  test("statement.assignment.let.manyVars") {
    Assignment("foo", Seq(),
      Let(
        Seq(
          Assignment("y", Seq(), astVar("x", Number)),
          Assignment("a", Seq(), astVar("b", Number))
        ),
        astBinOp(astVar("a", Number), Plus, astVar("y", Number))
      ) annotated Number
    ) ~>
    Seq(
      Assignment("_let0y", Seq(), astVar("x", Number)),
      Assignment("_let0a", Seq(), astVar("b", Number)),
      Assignment("foo", Seq(), astBinOp(astVar("_let0a", Number), Plus, astVar("_let0y", Number)))
    )
  }

  test("statement.assignment.let.many") {
    Seq(
      Assignment("foo", Seq(), Let(Seq(
        Assignment("y", Seq(), astVar("x", Number))),
        astVar("y", Number)
      ) annotated Number),
      Assignment("bar", Seq(), Let(Seq(
        Assignment("y", Seq(), astVar("z", Number))),
        astVar("y", Number)
      ) annotated Number)
    ) ~>
    Seq(
      Assignment("_let0y", Seq(), astVar("x", Number)),
      Assignment("foo", Seq(), astVar("_let0y", Number)),
      Assignment("_let1y", Seq(), astVar("z", Number)),
      Assignment("bar", Seq(), astVar("_let1y", Number))
    )
  }

  test("statement.assignment.let.shadowsOuterScope") {
    Seq(
      Assignment("x", Seq(), astNum(1)),
      Assignment("foo", Seq(),
        Let(
          Seq(Assignment("x", Seq(), astNum(2))),
          astVar("x", Number)
        ) annotated Number
      )
    ) ~>
    Seq(
      Assignment("x", Seq(), astNum(1)),
      Assignment("_let0x", Seq(), astNum(2)),
      Assignment("foo", Seq(), astVar("_let0x", Number))
    )
  }

  test("statement.assignment.let.bindingShadowsParam") {
    Assignment("foo", Seq("y"), Let(Seq(
      Assignment("y", Seq(), astVar("x", Number))),
      astVar("y", Number)
    ) annotated Number) ~>
    Seq(
      Assignment("_let0y", Seq(), astVar("x", Number)),
      Assignment("foo", Seq("y"), astVar("_let0y", Number))
    )
  }

  test("statement.assignment.let.paramShadowsBinding") {
    Assignment("foo", Seq(),
      Let(
        Seq(
          Assignment("x", Seq(), astNum(1)),
          Assignment("s", Seq("x"), astBinOp(astNum(1), Plus, astVar("x", Number)))
        ),
        Application("s", Seq(astVar("x", Number))) annotated Number
      ) annotated Number
    ) ~>
    Seq(
      Assignment("_let0x", Seq(), astNum(1)),
      Assignment("_let0s", Seq("x"), astBinOp(astNum(1), Plus, astVar("x", Number))),
      Assignment("foo", Seq(), Application("_let0s", Seq(astVar("_let0x", Number))) annotated Number)
    )
  }

  test("statement.assignment.number") {
    val asn = Assignment("foo", Seq(), astNum(1))
    asn ~> asn
  }

  test("statement.assignment.nested.binOp") {
    Assignment("foo", Seq(),
      astBinOp(astNum(1),
        Plus,
        Let(
          Seq(Assignment("s", Seq("x"), astBinOp(astVar("x", Number), Plus, astNum(1)))),
          Application("s", Seq(astNum(2))) annotated Number
        )
      )
    ) ~>
    Seq(
      Assignment("_let0s", Seq("x"), astBinOp(astVar("x", Number), Plus, astNum(1))),
      Assignment("foo", Seq(), astBinOp(astNum(1), Plus, Application("_let0s", Seq(astNum(2)))))
    )
  }

  test("statement.assignment.nested.chain") {
    Assignment("foo", Seq(), Chain(Seq(
      Let(
        Seq(Assignment("localComp", Seq(), astVar("globalComp", Source))),
        astVar("localComp", Source)
      ) annotated Source
    ))) ~>
    Seq(
      Assignment("_let0localComp", Seq(), astVar("globalComp", Source)),
      Assignment("foo", Seq(), Chain(Seq(astVar("_let0localComp", Source))))
    )
  }

  test("statement.assignment.nested.parallel") {
    Assignment("foo", Seq(), Parallel(Seq(
      Let(
        Seq(Assignment("localComp", Seq(), astVar("globalComp", Source))),
        astVar("localComp", Source)
      ) annotated Source
    ))) ~>
    Seq(
      Assignment("_let0localComp", Seq(), astVar("globalComp", Source)),
      Assignment("foo", Seq(), Parallel(Seq(astVar("_let0localComp", Source))))
    )
  }

  test("statement.assignment.nested.application") {
    Assignment("foo", Seq(),
      Application("bar",
        Seq(
          Let(
            Seq(Assignment("y", Seq(), astVar("x", Number))),
            astVar("y", Number)
          ) annotated Number
        )
      ) annotated Source
    ) ~>
    Seq(
      Assignment("_let0y", Seq(), astVar("x", Number)),
      Assignment("foo", Seq(), Application("bar", Seq(astVar("_let0y", Number))) annotated Source)
    )
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Instrument tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  test("statement.instrument.let.channel") {
    Instrument(
      Seq(
        Let(
          Seq(Assignment("x", Seq(), astNum(1))),
          astVar("x", Number)
        ) annotated Number
      ),
      astVar("source", Source)
    )~>
    Seq(
      Assignment("_let0x", Seq(), astNum(1)),
      Instrument(Seq(astVar("_let0x", Number)), astVar("source", Source))
    )
  }

  test("statement.instrument.let.body") {
    Instrument(
      Seq(astNum(1)),
      Let(
        Seq(Assignment("s", Seq(), astVar("source", Source))),
        astVar("s", Source)
      ) annotated Source
    ) ~>
    Seq(
      Assignment("_let0s", Seq(), astVar("source", Source)),
      Instrument(Seq(astNum(1)), astVar("_let0s", Source))
    )
  }

}
