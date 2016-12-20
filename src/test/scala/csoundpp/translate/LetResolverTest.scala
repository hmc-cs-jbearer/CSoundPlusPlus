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
      Assignment("let0y", Seq(), astVar("_x", Number)),
      Assignment("_foo", Seq(), astVar("let0y", Number))
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
      Assignment("let0y", Seq(), astVar("_x", Number)),
      Assignment("let0a", Seq(), astVar("_b", Number)),
      Assignment("_foo", Seq(), astBinOp(astVar("let0a", Number), Plus, astVar("let0y", Number)))
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
      Assignment("let0y", Seq(), astVar("_x", Number)),
      Assignment("_foo", Seq(), astVar("let0y", Number)),
      Assignment("let1y", Seq(), astVar("_z", Number)),
      Assignment("_bar", Seq(), astVar("let1y", Number))
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
      Assignment("_x", Seq(), astNum(1)),
      Assignment("let0x", Seq(), astNum(2)),
      Assignment("_foo", Seq(), astVar("let0x", Number))
    )
  }

  test("statement.assignment.let.bindingShadowsParam") {
    Assignment("foo", Seq("y"), Let(Seq(
      Assignment("y", Seq(), astVar("x", Number))),
      astVar("y", Number)
    ) annotated Number) ~>
    Seq(
      Assignment("let0y", Seq(), astVar("_x", Number)),
      Assignment("_foo", Seq("_y"), astVar("let0y", Number))
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
      Assignment("let0x", Seq(), astNum(1)),
      Assignment("let0s", Seq("_x"), astBinOp(astNum(1), Plus, astVar("_x", Number))),
      Assignment("_foo", Seq(), Application("let0s", Seq(astVar("let0x", Number))) annotated Number)
    )
  }

  test("statement.assignment.number") {
    Assignment("foo", Seq(), astNum(1)) ~> Assignment("_foo", Seq(), astNum(1))
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
      Assignment("let0s", Seq("_x"), astBinOp(astVar("_x", Number), Plus, astNum(1))),
      Assignment("_foo", Seq(), astBinOp(astNum(1), Plus, Application("let0s", Seq(astNum(2)))))
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
      Assignment("let0localComp", Seq(), astVar("_globalComp", Source)),
      Assignment("_foo", Seq(), Chain(Seq(astVar("let0localComp", Source))))
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
      Assignment("let0localComp", Seq(), astVar("_globalComp", Source)),
      Assignment("_foo", Seq(), Parallel(Seq(astVar("let0localComp", Source))))
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
      Assignment("let0y", Seq(), astVar("_x", Number)),
      Assignment("_foo", Seq(), Application("_bar", Seq(astVar("let0y", Number))) annotated Source)
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
      Assignment("let0x", Seq(), astNum(1)),
      Instrument(Seq(astVar("let0x", Number)), astVar("_source", Source))
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
      Assignment("let0s", Seq(), astVar("_source", Source)),
      Instrument(Seq(astNum(1)), astVar("let0s", Source))
    )
  }

}
