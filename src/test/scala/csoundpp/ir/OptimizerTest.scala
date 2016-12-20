package cspp

import scala.language.implicitConversions
import org.scalatest.FunSuite
import org.scalatest.Matchers

import absyn._
import AbsynSugar._

class OptimizerSuite extends FunSuite with Matchers {

  def optimize(input: Seq[Statement]) = CsppOptimizer(input) match {
    case Left(err) => fail(s"Failed to optimize $input: $err.")
    case Right(output) => output
  }

  implicit class AstTester(input: Seq[Statement]) {
    def ~>(output: Seq[Statement]) = optimize(input) should equal (output)
  }

  implicit class StmtTester(input: Statement) {
    def ~>(output: Statement) = Seq(input) ~> Seq(output)
  }

  implicit class ExprTester(input: Expr) {
    // Make a dummy statement which relies on this expression
    def ~>(output: Expr) =
      Seq(Assignment("foo", Seq(), input)) ~> optimize(Seq(Assignment("foo", Seq(), output)))
  }

  def astVar(name: String) = Application(name, Seq())

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Generic tests: preserves type annotations
  //////////////////////////////////////////////////////////////////////////////////////////////////

  def tyTest(name: String, input: Expr) = test(name) {
    input.ty match {
      case Some(ty) => {
        val optimized = optimize(Seq(Assignment("foo", Seq(), input)))
        optimized.length should equal (1)
        val Assignment(_, _, output) = optimized.head
        output.ty should be (Some(ty))
      }

      case None => fail("Input expression is not annotated.")
    }
  }

  tyTest("typeAnnotations.number", Num(1) annotated Number)
  tyTest("typeAnnotations.binOp", BinOp(Num(1), Plus, Num(2)) annotated Number)
  test("typeAnnotations.binOp.subExpr") {
    val expr = BinOp(Num(1) annotated Number, Plus, astVar("foo") annotated Number) annotated Number
    expr ~> expr
  }

  tyTest("typeAnnotations.chain.source", Chain(Seq()) annotated Source)
  tyTest("typeAnnotations.chain.effect", Chain(Seq()) annotated Effect)
  test("typeAnnotations.chain.subExpr") {
    val expr = Chain(Seq(astVar("foo") annotated Source)) annotated Source
    expr ~> expr
  }

  tyTest("typeAnnotations.parallel.mux", Parallel(Seq()) annotated Component(2, 1))
  tyTest("typeAnnotations.parallel.demux", Parallel(Seq()) annotated Component(1, 2))
  test("typeAnnotations.parallel.subExpr") {
    val expr = Parallel(Seq(astVar("foo") annotated Source)) annotated Source
    expr ~> expr
  }

  tyTest("typeAnnotations.application.number", astVar("foo") annotated Number)
  tyTest("typeAnnotations.application.source", astVar("foo") annotated Source)
  tyTest("typeAnnotations.application.effect", astVar("foo") annotated Effect)
  test("typeAnnotations.application.subExpr") {
    val expr = Application("foo", Seq(Num(1) annotated Number)) annotated Source
    expr ~> expr
  }

  test("typeAnnotations.let") {
    val expr = Let(Seq(
      Assignment("foo", Seq(), Num(1) annotated Number)),
      Application("bar", Seq(astVar("foo") annotated Number)) annotated Source)
    expr ~> expr
  }

  test("typeAnnotations.statement.assignment") {
    val stmt = Assignment("foo", Seq(),
      BinOp(astVar("bar") annotated Number, Plus, Num(1) annotated Number) annotated Number)
    stmt ~> stmt
  }

  test("typeAnnotations.statement.instrument") {
    val stmt = Instrument(Seq(Num(1) annotated Number),
      Chain(Seq(astVar("foo") annotated Source)) annotated Source)
    stmt ~> stmt
  }

  test("typeAnnotations.statement.sends") {
    val stmt = Instrument(Seq(Num(1) annotated Number),
      astVar("foo") annotated Source,
      Some(astVar("bar") annotated Effect)
    )
    stmt ~> stmt
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Constant folding tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  test("constantFolding.binOp.plus") {
    BinOp(Num(1), Plus, Num(2)) ~> Num(3)
  }

  test("constantFolding.binOp.minus") {
    BinOp(Num(2), Minus, Num(1)) ~> Num(1)
  }

  test("constantFolding.binOp.times") {
    BinOp(Num(2), Times, Num(3)) ~> Num(6)
  }

  test("constantFolding.binOp.divide") {
    BinOp(Num(6), Divide, Num(3)) ~> Num(2)
  }

  test("constantFolding.binOp.nested.left") {
    BinOp(BinOp(Num(1), Plus, Num(2)), Times, Num(3)) ~> Num(9)
  }

  test("constantFolding.binOp.nested.right") {
    BinOp(Num(3), Times, BinOp(Num(2), Plus, Num(1))) ~> Num(9)
  }

  test("constantFolding.binOp.nested.both") {
     BinOp(BinOp(Num(1), Plus, Num(2)), Times, BinOp(Num(3), Plus, Num(4))) ~> Num(21)
  }

  test("constantFolding.binOp.irreducible.left") {
    val expr = BinOp(BinOp(astVar("foo"), Plus, Num(2)), Times, Num(3))
    expr ~> expr
  }

  test("constantFolding.binOp.irreducible.right") {
    BinOp(BinOp(Num(1), Plus, Num(2)), Times, astVar("foo")) ~> BinOp(Num(3), Times, astVar("foo"))
  }

  test("constantFolding.binOp.irreducible.both") {
    val expr = BinOp(astVar("foo"), Plus, astVar("bar"))
    expr ~> expr
  }

  test("constantFolding.binOp.type") {
    (BinOp(Num(1), Plus, Num(2)) annotated Number) ~> (Num(3) annotated Number)
  }

}
