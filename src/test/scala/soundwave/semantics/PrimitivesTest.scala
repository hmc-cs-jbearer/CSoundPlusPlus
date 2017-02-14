package soundwave

import org.scalatest.{FunSuite, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import absyn._
import AbsynSugar._

class PrimitivesSuite extends FunSuite with GeneratorDrivenPropertyChecks with Matchers {

  val epsilon = 1e-12f

  test("sqrt") {
    forAll("x") { (x: Double) =>
      whenever (x >= 0) {
        SwPrimitives.call("sqrt", Seq(x)) match {
          case Num(n, _)  => n shouldBe Math.sqrt(x) +- epsilon
          case expr       => fail(s"Expected Num(${Math.sqrt(x)}, but got $expr.")
        }
      }
    }
  }

}
