package soundwave

import org.scalatest.compatible.Assertion
import org.scalatest.FunSuite
import org.scalatest.Matchers

import absyn._
import AbsynSugar._

class LetResolverSuite extends FunSuite with Matchers {

  def test(input: Seq[Statement], expected: Seq[Statement]) = SwLetResolver(input) match {
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

  def astVar(name: Ident, ty: SwType) = Application(name, Seq()) annotated ty

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

  test("statement.assignment.let.shadows.outerScope") {
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

  test("statement.assignment.let.shadows.binding") {
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

  test("statement.assignment.let.captures.global.variable") {
    Seq(
      Assignment("x", Seq(), astNum(1)),
      Assignment("foo", Seq(),
        Let(
          Seq(Assignment("y", Seq(), astVar("x", Number))),
          astVar("y", Number)
        ) annotated Number
      )
    ) ~>
    Seq(
      Assignment("x", Seq(), astNum(1)),
      Assignment("_let0y", Seq(), astVar("x", Number)),
      Assignment("foo", Seq(), Application("_let0y", Seq()) annotated Number)
    )
  }

  test("statement.assignment.let.captures.global.function") {
    Seq(
      Assignment("add", Seq("x", "y"), BinOp(astVar("x", Number), Plus, astVar("y", Number))),
      Assignment("foo", Seq(),
        Let(
          Seq(Assignment("s", Seq("x"), Application("add", Seq(astVar("x", Number), astNum(1))) annotated Number)),
          Application("s", Seq(astNum(41))) annotated Number
        ) annotated Number
      )
    ) ~>
    Seq(
      Assignment("add", Seq("x", "y"), BinOp(astVar("x", Number), Plus, astVar("y", Number))),
      Assignment("_let0s", Seq("x"), Application("add", Seq(astVar("x", Number), astNum(1))) annotated Number),
      Assignment("foo", Seq(), Application("_let0s", Seq(astNum(41))) annotated Number)
    )
  }

  test("statement.assignment.let.captures.sameLet.variable") {
    Assignment("foo", Seq(),
      Let(
        Seq(
          Assignment("x", Seq(), astNum(1)),
          Assignment("y", Seq(), astVar("x", Number))
        ),
        astVar("y", Number)
      ) annotated Number
    ) ~>
    Seq(
      Assignment("_let0x", Seq(), astNum(1)),
      Assignment("_let0y", Seq(), astVar("_let0x", Number)),
      Assignment("foo", Seq(), astVar("_let0y", Number))
    )
  }

  test("statement.assignment.let.captures.sameLet.function") {
    Assignment("foo", Seq(),
      Let(
        Seq(
          Assignment("add", Seq("x", "y"), BinOp(astVar("x", Number), Plus, astVar("y", Number))),
          Assignment("s", Seq("x"), Application("add", Seq(astVar("x", Number), astNum(1))) annotated Number)
        ),
        Application("s", Seq(astNum(41))) annotated Number
      ) annotated Number
    ) ~>
    Seq(
      Assignment("_let0add", Seq("x", "y"), BinOp(astVar("x", Number), Plus, astVar("y", Number))),
      Assignment("_let0s", Seq("x"), Application("_let0add", Seq(astVar("x", Number), astNum(1))) annotated Number),
      Assignment("foo", Seq(), Application("_let0s", Seq(astNum(41))) annotated Number)
    )
  }

  test("statement.assignment.let.captures.outerLet.variable") {
    Assignment("foo", Seq(),
      Let(
        Seq(Assignment("x", Seq(), astNum(1))),
        Let(
          Seq(Assignment("y", Seq(), astVar("x", Number))),
          astVar("y", Number)
        ) annotated Number
      ) annotated Number
    ) ~>
    Seq(
      Assignment("_let0x", Seq(), astNum(1)),
      Assignment("_let1y", Seq(), astVar("_let0x", Number)),
      Assignment("foo", Seq(), astVar("_let1y", Number))
    )
  }

  test("statement.assignment.let.captures.outerLet.function") {
    Assignment("foo", Seq(),
      Let(
        Seq(Assignment("add", Seq("x", "y"), BinOp(astVar("x", Number), Plus, astVar("y", Number)))),
        Let(
          Seq(Assignment("s", Seq("x"), Application("add", Seq(astVar("x", Number), astNum(1))) annotated Number)),
          Application("s", Seq(astNum(41))) annotated Number
        ) annotated Number
      ) annotated Number
    ) ~>
    Seq(
      Assignment("_let0add", Seq("x", "y"), BinOp(astVar("x", Number), Plus, astVar("y", Number))),
      Assignment("_let1s", Seq("x"), Application("_let0add", Seq(astVar("x", Number), astNum(1))) annotated Number),
      Assignment("foo", Seq(), Application("_let1s", Seq(astNum(41))) annotated Number)
    )
  }

  test("statement.assignment.let.captures.params.distinctNames") {
    Assignment("foo", Seq("x"),
      Let(
        Seq(Assignment("y", Seq(), astVar("x", Number))),
        astVar("y", Number)
      ) annotated Number
    ) ~>
    Seq(
      Assignment("_let0y", Seq("x"), astVar("x", Number)),
      Assignment("foo", Seq("x"), Application("_let0y", Seq(astVar("x", Number))) annotated Number)
    )
  }

  test("statement.assignment.let.captures.params.shadows") {
    Assignment("foo", Seq("x", "y"),
      Let(
        Seq(Assignment("y", Seq(), astVar("x", Number))),
        astVar("y", Number)
      ) annotated Number
    ) ~>
    Seq(
      Assignment("_let0y", Seq("x", "y"), astVar("x", Number)),
      Assignment("foo", Seq("x", "y"), Application("_let0y", Seq(astVar("x", Number), astVar("y", Number))) annotated Number)
    )
  }

  test("statement.assignment.let.captures.params.functions") {
    Assignment("foo", Seq("x"),
      Let(
        Seq(
          Assignment("addX", Seq("y"), astBinOp(astVar("x", Number), Plus, astVar("y", Number))),
          Assignment("succX", Seq(), Application("addX", Seq(astNum(1))) annotated Number)
        ),
        astVar("succX", Number)
      ) annotated Number
    ) ~>
    Seq(
      Assignment("_let0addX", Seq("x", "y"), astBinOp(astVar("x", Number), Plus, astVar("y", Number))),
      Assignment("_let0succX", Seq("x"), Application("_let0addX", Seq(astVar("x", Number), astNum(1))) annotated Number),
      Assignment("foo", Seq("x"), Application("_let0succX", Seq(astVar("x", Number))) annotated Number)
    )
  }

  test("statement.assignment.let.captures.shadowedByParam") {
    Assignment("foo", Seq("x"),
      Let(
        Seq(Assignment("s", Seq("x"), astBinOp(astVar("x", Number), Plus, astNum(1)))),
        Application("s", Seq(astVar("x", Number))) annotated Number
      ) annotated Number
    ) ~>
    Seq(
      Assignment("_let0s", Seq("_x", "x"), astBinOp(astVar("x", Number), Plus, astNum(1))),
      Assignment("foo", Seq("x"), Application("_let0s", Seq(astVar("x", Number), astVar("x", Number))) annotated Number)
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

  def instrVar(name: Ident, ty: SwType) =
    Application(name, Seq(astVar("amp", Number), astVar("freq", Number))) annotated ty

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
      Assignment("_let0s", Seq("amp", "freq"), astVar("source", Source)),
      Instrument(Seq(astNum(1)), instrVar("_let0s", Source))
    )
  }

  test("statement.instrument.let.sends") {
    Instrument(
      Seq(astNum(1)),
      astVar("source", Source),
      Some(
        Let(
          Seq(Assignment("e", Seq(), astVar("effect", Effect))),
          astVar("e", Effect)
        ) annotated Effect
      )
    )~>
    Seq(
      Assignment("_let0e", Seq("amp", "freq"), astVar("effect", Effect)),
      Instrument(Seq(astNum(1)), astVar("source", Source), Some(instrVar("_let0e", Effect)))
    )
  }

  test("statement.instrument.captures.midiParams.distinctNames") {
    Instrument(
      Seq(astNum(1)),
      Let(
        Seq(
          Assignment("x", Seq(), astVar("amp", Number)),
          Assignment("y", Seq(), astVar("freq", Number))
        ),
        Application("source", Seq(astVar("x", Number), astVar("y", Number))) annotated Source
      ) annotated Source
    ) ~>
    Seq(
      Assignment("_let0x", Seq("amp", "freq"), astVar("amp", Number)),
      Assignment("_let0y", Seq("amp", "freq"), astVar("freq", Number)),
      Instrument(
        Seq(astNum(1)),
        Application("source", Seq(
          Application("_let0x", Seq(astVar("amp", Number), astVar("freq", Number))) annotated Number,
          Application("_let0y", Seq(astVar("amp", Number), astVar("freq", Number))) annotated Number
        )) annotated Source
      )
    )
  }

  test("statement.instrument.captures.midiParams.shadows") {
    Instrument(
      Seq(astNum(1)),
      Let(
        Seq(
          Assignment("amp", Seq(), astVar("freq", Number)) // OK, so this is a bit contrived...
        ),
        Application("source", Seq(astVar("amp", Number))) annotated Source
      ) annotated Source
    ) ~>
    Seq(
      Assignment("_let0amp", Seq("amp", "freq"), astVar("freq", Number)),
      Instrument(
        Seq(astNum(1)),
        Application("source", Seq(
          Application("_let0amp", Seq(astVar("amp", Number), astVar("freq", Number))) annotated Number
        )) annotated Source
      )
    )
  }

  test("statement.instrument.captures.midiParams.functions") {
    Instrument(
      Seq(astNum(1)),
      Let(
        Seq(
          Assignment("scaleAmp", Seq("x"), astBinOp(astVar("amp", Number), Times, astVar("x", Number))),
          Assignment("product", Seq(), Application("scaleAmp", Seq(astVar("freq", Number))) annotated Number)
        ),
        Application("source", Seq(astVar("product", Number))) annotated Source
      ) annotated Source
    ) ~>
    Seq(
      Assignment("_let0scaleAmp", Seq("amp", "freq", "x"), astBinOp(astVar("amp", Number), Times, astVar("x", Number))),
      Assignment("_let0product", Seq("amp", "freq"), Application("_let0scaleAmp",
        Seq(astVar("amp", Number), astVar("freq", Number), astVar("freq", Number))) annotated Number),
      Instrument(
        Seq(astNum(1)),
        Application("source",
          Seq(
            Application("_let0product", Seq(astVar("amp", Number), astVar("freq", Number))) annotated Number
          )
        ) annotated Source
      )
    )
  }

  test("statement.instrument.sends.captures.midiParams.distinctNames") {
    Instrument(
      Seq(astNum(1)),
      astVar("source", Source),
      Some(
        Let(
          Seq(
            Assignment("x", Seq(), astVar("amp", Number)),
            Assignment("y", Seq(), astVar("freq", Number))
          ),
          Application("effect", Seq(astVar("x", Number), astVar("y", Number))) annotated Effect
        ) annotated Effect
      )
    ) ~>
    Seq(
      Assignment("_let0x", Seq("amp", "freq"), astVar("amp", Number)),
      Assignment("_let0y", Seq("amp", "freq"), astVar("freq", Number)),
      Instrument(
        Seq(astNum(1)),
        astVar("source", Source),
        Some(
          Application("effect", Seq(
            Application("_let0x", Seq(astVar("amp", Number), astVar("freq", Number))) annotated Number,
            Application("_let0y", Seq(astVar("amp", Number), astVar("freq", Number))) annotated Number
          )) annotated Effect
        )
      )
    )
  }

  test("statement.instrument.sends.captures.midiParams.shadows") {
    Instrument(
      Seq(astNum(1)),
      astVar("source", Source),
      Some(
        Let(
          Seq(
            Assignment("amp", Seq(), astVar("freq", Number)) // OK, so this is a bit contrived...
          ),
          Application("effect", Seq(astVar("amp", Number))) annotated Effect
        ) annotated Effect
      )
    ) ~>
    Seq(
      Assignment("_let0amp", Seq("amp", "freq"), astVar("freq", Number)),
      Instrument(
        Seq(astNum(1)),
        astVar("source", Source),
        Some(
          Application("effect", Seq(
            Application("_let0amp", Seq(astVar("amp", Number), astVar("freq", Number))) annotated Number
          )) annotated Effect
        )
      )
    )
  }

  test("statement.instrument.sends.captures.midiParams.functions") {
    Instrument(
      Seq(astNum(1)),
      astVar("source", Source),
      Some(
        Let(
          Seq(
            Assignment("scaleAmp", Seq("x"), astBinOp(astVar("amp", Number), Times, astVar("x", Number))),
            Assignment("product", Seq(), Application("scaleAmp", Seq(astVar("freq", Number))) annotated Number)
          ),
          Application("effect", Seq(astVar("product", Number))) annotated Effect
        ) annotated Effect
      )
    ) ~>
    Seq(
      Assignment("_let0scaleAmp", Seq("amp", "freq", "x"), astBinOp(astVar("amp", Number), Times, astVar("x", Number))),
      Assignment("_let0product", Seq("amp", "freq"), Application("_let0scaleAmp",
        Seq(astVar("amp", Number), astVar("freq", Number), astVar("freq", Number))) annotated Number),
      Instrument(
        Seq(astNum(1)),
        astVar("source", Source),
        Some(
          Application("effect",
            Seq(
              Application("_let0product", Seq(astVar("amp", Number), astVar("freq", Number))) annotated Number
            )
          ) annotated Effect
        )
      )
    )
  }

}
