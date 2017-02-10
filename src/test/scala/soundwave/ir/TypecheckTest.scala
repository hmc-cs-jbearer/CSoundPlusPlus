package soundwave

import scala.language.implicitConversions
import org.scalatest.FunSuite
import org.scalatest.Matchers

import absyn._
import AbsynSugar._
import tokens._

class TypecheckSuite extends FunSuite with Matchers {

  type PrimitiveMap = SwTypeChecker.TypeMap

  def builtin(mappings: ((String, Int), SwType)*): PrimitiveMap =
    new PrimitiveMap() ++ (mappings collect {
      case ((name, arity), ty) => ((Ident(name), arity), ty)
    })

  // Syntactic sugar for 0-argument functions (ie variables)
  def Var(id: Ident) = Application(id, Seq())
  def Assign(id: Ident, body: Expr) = Assignment(id, Seq(), body)

  def testGoodInput(builtins: PrimitiveMap, input: Seq[Statement], expected: Seq[Statement]) = {
    SwTypeChecker(builtins, input) match {
      case Right(output) => {
        // The exact order is undefined, so we compare them as sets. To check a partial ordering,
        // use `ordered(("id1", arity) before ("id2", arity))`
        output.toSet should equal (expected.toSet)

        // We can check that the assignments all come before the instruments
        var seenInstr = false
        for (stmt <- output) {
          if (seenInstr && stmt.isInstanceOf[Assignment]) {
            fail(s"Found assignment after instrument in $output")
          }
          if (!seenInstr && stmt.isInstanceOf[Instrument]) {
            seenInstr = true
          }
        }

        output
      }
      case Left(err) => {
        fail(/*input.toString ++ " failed to typecheck: " ++*/ err.getMessage)
        Seq()
      }
    }
  }

  def testBadInput(builtins: PrimitiveMap, input: Seq[Statement], loc: Location) =
  SwTypeChecker(builtins, input) match {
    case Right(output)                          => fail(output.toString ++ " was successful.")
    case Left(SwCompileError(reportedLoc, _)) => reportedLoc should equal (loc)
  }

  def testBadInput(builtins: PrimitiveMap, input: Seq[Statement]) =
    SwTypeChecker(builtins, input) match {
      case Right(output)  => fail(output.toString ++ " was successful.")
      case Left(_)        => ()
    }

  // Test a whole program starting from a string, and going through the lexing, parsing, and
  // typecheck phases. This is used to test the locations reported with type errors
  class SystemTester(builtins: PrimitiveMap, input: String) {
    def lexAndThen[T](continue: Seq[SwToken] => T)(input: String) = {
      SwLexer(input) match {
        case Right(output)                            => continue(output)
        case Left(SwCompileError(reportedLoc, msg)) =>
          fail("Lexer error in typecheck test program. Possible regression in unit tests." ++
               reportedLoc.toString ++ ": " ++ msg)
      }
    }

    def parseAndThen[T](continue: Seq[Statement] => T)(input: Seq[SwToken]) = {
      SwParser(input) match {
        case Right(output)                            => continue(output)
        case Left(SwCompileError(reportedLoc, msg)) =>
          fail("Parse error in typecheck test program. Possible regression in unit tests." ++
               reportedLoc.toString ++ ": " ++ msg)
      }
    }

    def ~>(output: Seq[Statement]) =
      lexAndThen {
        parseAndThen { (ast: Seq[Statement]) =>
          builtins ~> ast ~> output
        }
      } (input)

    def ~>(error: TypeError) =
      lexAndThen {
        parseAndThen { (ast: Seq[Statement]) =>
          builtins ~> ast ~> error
        }
      } (input)
  }

  class ProgramTester(val builtins: PrimitiveMap, val input: Seq[Statement]) {
    def ~>(output: Seq[Statement]) = new OrderingAssertible(testGoodInput(builtins, input, output))
    def ~>(error: TypeError) = testBadInput(builtins, input, error)
    def ~/>(output: ExpectFailure) = testBadInput(builtins, input)
  }

  class OrderingAssertible(val stmts: Seq[Statement]) {
    def ordered(orderings: ((Ident, Int), (Ident, Int))*) =
      for ((first, second) <- orderings) {
        val firstIndex = stmts.indexWhere(_ match {
          case Assignment(id, params, _) => (id, params.length) == first
          case _                         => false
        })
        val secondIndex = stmts.indexWhere(_ match {
          case Assignment(id, params, _) => (id, params.length) == second
          case _                         => false
        })
        if (firstIndex == -1) {
          fail(s"Check your test case: $first did not occur at all in $stmts.")
        }
        if (secondIndex == -1) {
          fail(s"Check your test case: $second did not occur at all in $stmts.")
        }
        if (firstIndex >= secondIndex) {
          fail(s"$first did not occur before $second in $stmts")
        }
      }
  }

  implicit class OrderingAssertion(first: (String, Int)) {
    def before(second: (String, Int)) = ((Ident(first._1), first._2), (Ident(second._1), second._2))
  }

  class StmtTester(builtins: PrimitiveMap, input: Statement) {
    def ~>(output: Statement) = builtins ~> Seq(input) ~> Seq(output)
    def ~>(error: TypeError) = builtins ~> Seq(input) ~> error
    def ~/>(output: ExpectFailure) = builtins ~> Seq(input) ~/> output
  }

  class ExprTester(builtins: PrimitiveMap, input: Expr) {
    def ~>(output: Expr) = builtins ~>
      // Wrap the expression in a bogus assignment
      Assignment("_unique_name", Seq(), input) ~> Assignment("_unique_name", Seq(), output)

    def ~>(error: TypeError) = builtins ~>
      // Wrap the expression in a bogus assignment
      Assignment("_unique_name", Seq(), input) ~> error

    def ~/>(output: ExpectFailure) = builtins ~>
      // Wrap the expression in a bogus assignment
      Assignment("_unique_name", Seq(), input) ~/> output
  }

  implicit class TypecheckTestBuilder(builtins: PrimitiveMap) {
    def ~>(input: Seq[Statement]) = new ProgramTester(builtins, input)
    def ~>(input: Statement) = new StmtTester(builtins, input)
    def ~>(input: Expr) = new ExprTester(builtins, input)
    def ~>(input: String) = new SystemTester(builtins, input)
  }

  // Overload selectors for tests that expect the type check to fail
  class ExpectFailure
  object ident extends ExpectFailure
  object expr extends ExpectFailure
  object statement extends ExpectFailure
  object program extends ExpectFailure

  // Object used to state expectation for tests that should fail the typecheck phase
  class TypeError(line: Int, column: Int) extends Location(line, column, "")

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Expression tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  test("expression.num") {
    val expr = Num(42)
    builtin() ~> expr ~> (expr annotated Number)
  }

  test("expression.var.num") {
    val expr = Var(Ident("foo"))
    builtin(("foo", 0) -> Number) ~> expr ~> (expr annotated Number)
  }

  test("expression.var.source") {
    val expr = Var(Ident("foo"))
    builtin(("foo", 0) -> Source) ~> expr ~> (expr annotated Source)
  }

  test("expression.var.effect") {
    val expr = Var(Ident("foo"))
    builtin(("foo", 0) -> Effect) ~> expr ~> (expr annotated Effect)
  }

  test("expression.var.invalid.unknown") {
    builtin() ~> Var(Ident("foo")) ~/> expr
  }

  test("expression.function.number") {
    val expr = Application(Ident("foo"), Seq(Num(42)))
    val annotated = Application(Ident("foo"), Seq(Num(42) annotated Number)) annotated Number
    builtin(("foo", 1) -> Number) ~> expr ~> annotated
  }

  test("expression.function.invalid.nonnumericArgs") {
    builtin(("foo", 1) -> Number, ("source", 0) -> Source) ~>
      Application(Ident("foo"), Seq(Var("source"))) ~/> expr
  }

  test("expression.chain.empty") {
    builtin() ~> Chain(Seq()) ~> (Chain(Seq()) annotated Effect)
  }

  test("expression.chain.sourceOnly") {
    val comp = Application(Ident("source"), Seq())
    val expr = Chain(Seq(comp))
    val annotated = Chain(Seq(comp annotated Source)) annotated Source
    builtin(("source", 0) -> Source) ~> expr ~> annotated
  }

  test("expression.chain.effectOnly") {
    val comp = Application(Ident("effect"), Seq())
    val expr = Chain(Seq(comp))
    val annotated = Chain(Seq(comp annotated Effect)) annotated Effect
    builtin(("effect", 0) -> Effect) ~> expr ~> annotated
  }

  test("expression.chain.sourceAndEffect") {
    val source = Application(Ident("source"), Seq())
    val effect = Application(Ident("effect"), Seq())
    val expr = Chain(Seq(source, effect))
    val annotated = Chain(Seq(source annotated Source, effect annotated Effect)) annotated Source
    builtin(("source", 0) -> Source, ("effect", 0) -> Effect) ~> expr ~> annotated
  }

  test("expression.chain.invalid.twoSource") {
    val source1 = Application(Ident("source1"), Seq())
    val source2 = Application(Ident("source2"), Seq())
    val input = Chain(Seq(source1, source2))
    builtin(("source1", 0) -> Source, ("source2", 0) -> Source) ~> input ~/> expr
  }

  test("expression.chain.invalid.effectAndSource") {
    val source = Application(Ident("source"), Seq())
    val effect = Application(Ident("effect"), Seq())
    val input = Chain(Seq(effect, source))
    builtin(("source", 0) -> Source, ("effect", 0) -> Effect) ~> input ~/> expr
  }

  test("expression.chain.invalid.number") {
    builtin(("var", 0) -> Number) ~> Chain(Seq(Var(Ident("var")))) ~/> expr
  }

  test("expression.parallel.empty") {
    builtin() ~> Parallel(Seq()) ~> (Parallel(Seq()) annotated Effect)
  }

  test("expression.parallel.sourceOnly") {
    val comp = Application(Ident("source"), Seq())
    val expr = Parallel(Seq(comp))
    val annotated = Parallel(Seq(comp annotated Source)) annotated Source
    builtin(("source", 0) -> Source) ~> expr ~> annotated
  }

  test("expression.parallel.effectOnly") {
    val comp = Application(Ident("effect"), Seq())
    val expr = Parallel(Seq(comp))
    val annotated = Parallel(Seq(comp annotated Effect)) annotated Effect
    builtin(("effect", 0) -> Effect) ~> expr ~> annotated
  }

  test("expression.parallel.sourceAndEffect") {
    val source = Application(Ident("source"), Seq())
    val effect = Application(Ident("effect"), Seq())
    val expr = Parallel(Seq(source, effect))
    val annotated =
      Parallel(Seq(source annotated Source, effect annotated Effect)) annotated Component(1, 2)
    builtin(("source", 0) -> Source, ("effect", 0) -> Effect) ~> expr ~> annotated
  }

  test("expression.parallel.twoSource") {
    val source1 = Application(Ident("source1"), Seq())
    val source2 = Application(Ident("source2"), Seq())
    val input = Parallel(Seq(source1, source2))
    val annotated =
      Parallel(Seq(source1 annotated Source, source2 annotated Source)) annotated Component(0, 2)
    builtin(("source1", 0) -> Source, ("source2", 0) -> Source) ~> input ~> annotated
  }

  test("expression.parallel.effectAndSource") {
    val source = Application(Ident("source"), Seq())
    val effect = Application(Ident("effect"), Seq())
    val input = Parallel(Seq(effect, source))
    val annotated =
      Parallel(Seq(effect annotated Effect, source annotated Source)) annotated Component(1, 2)
    builtin(("source", 0) -> Source, ("effect", 0) -> Effect) ~> input ~> annotated
  }

  test("expression.parallel.twoEffect") {
    val effect1 = Application(Ident("effect1"), Seq())
    val effect2 = Application(Ident("effect2"), Seq())
    val input = Parallel(Seq(effect1, effect2))
    val annotated =
      Parallel(Seq(effect1 annotated Effect, effect2 annotated Effect)) annotated Component(2, 2)
    builtin(("effect1", 0) -> Effect, ("effect2", 0) -> Effect) ~> input ~> annotated
  }

  test("expression.parallel.effectsAndSources") {
    val source1 = Application(Ident("source1"), Seq())
    val source2 = Application(Ident("source2"), Seq())
    val effect1 = Application(Ident("effect1"), Seq())
    val effect2 = Application(Ident("effect2"), Seq())
    val input = Parallel(Seq(source1, source2, effect1, effect2))
    val annotated =
      Parallel(Seq(
        source1 annotated Source,
        source2 annotated Source,
        effect1 annotated Effect,
        effect2 annotated Effect)
      ) annotated Component(2, 4)
    builtin(("source1", 0) -> Source,
            ("source2", 0) -> Source,
            ("effect1", 0) -> Effect,
            ("effect2", 0) -> Effect) ~> input ~> annotated
  }

  test("expression.parallel.invalid.number") {
    builtin(("var", 0) -> Number) ~> Parallel(Seq(Var(Ident("var")))) ~/> expr
  }

  test("expression.binop.numbers") {
    val lhs = Num(42)
    val rhs = Num(1)
    val expr = BinOp(lhs, Plus, rhs)
    val annotated = BinOp(lhs annotated Number, Plus, rhs annotated Number) annotated Number
    builtin() ~> expr ~> annotated
  }

  test("expression.binop.functions") {
    val lhs = Var(Ident("foo"))
    val rhs = Var(Ident("bar"))
    val expr = BinOp(lhs, Plus, rhs)
    val annotated = BinOp(lhs annotated Number, Plus, rhs annotated Number) annotated Number
    builtin(("foo", 0) -> Number, ("bar", 0) -> Number) ~> expr ~> annotated
  }

  test("expression.binop.invalid.source") {
    builtin(("source", 0) -> Source) ~> BinOp(Num(42), Plus, Var("source")) ~/> expr
  }

  test("expression.let.newBinding") {
    val binding = Assignment("x", Seq(), Num(5))
    val expr = Var("x")
    val let = Let(Seq(binding), expr)
    val annotatedLet = Let(
      Seq(Assignment("x", Seq(), Num(5) annotated Number)),
      expr annotated Number
    ) annotated Number
    builtin() ~> let ~> annotatedLet
  }

  test("expression.let.shadow") {
    val binding = Assignment("x", Seq(), Num(5))
    val expr = Var("x")
    val let = Let(Seq(binding), expr)
    val annotatedLet = Let(
      Seq(Assignment("x", Seq(), Num(5) annotated Number)),
      expr annotated Number
    ) annotated Number
    builtin(("x", 0) -> Source) ~> let ~> annotatedLet
  }

  test("expression.let.immediatelyInScope") {
    val binding1 = Assignment("x", Seq(), Num(5))
    val annotatedBinding1 = Assignment("x", Seq(), Num(5) annotated Number)

    val binding2 = Assignment("y", Seq(), Var("x"))
    val annotatedBinding2 = Assignment("y", Seq(), Var("x") annotated Number)

    val expr = Var("y")
    val let = Let(Seq(binding1, binding2), expr)
    val annotatedLet =
      Let(Seq(annotatedBinding1, annotatedBinding2), expr annotated Number) annotated Number
    builtin() ~> let ~> annotatedLet
  }

  test("expression.let.function") {
    val binding = Assignment("s", Seq("x"), BinOp(Var("x"), Plus, Num(1)))
    val annotatedBinding = Assignment("s", Seq("x"),
      BinOp(Var("x") annotated Number, Plus, Num(1) annotated Number) annotated Number)
    val expr = Application("s", Seq(Num(5)))
    val annotatedExpr = Application("s", Seq(Num(5) annotated Number)) annotated Number
    val let = Let(Seq(binding), expr)
    val annotatedLet = Let(Seq(annotatedBinding), annotatedExpr) annotated Number
    builtin() ~> let ~> annotatedLet
  }

  test("expression.let.invalid.redefiniton") {
    builtin() ~> Let(Seq(
      Assignment("x", Seq(), Num(5)),
      Assignment("x", Seq(), Num(6))
    ), Var("x")) ~/> expr
  }

  test("expression.let.invalid.unknown.inBinding") {
    builtin() ~> Let(Seq(Assignment("x", Seq(), Var("y"))), Var("x")) ~/> expr
  }

  test("expression.let.invalid.unknown.inBody") {
    builtin() ~> Let(Seq(Assignment("x", Seq(), Num(5))), Var("y")) ~/> expr
  }

  test("expression.let.invalid.useAfter") {
    builtin() ~> Seq(
      Assign("foo", Let(Seq(Assign("x", Num(1))), Var("x"))),
      Assign("bar", Var("x"))
    ) ~/> program
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Statement tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  test("statement.assignment.num") {
    val expr = Num(42)
    val stmt = Assign(Ident("foo"), expr)
    val annotated = Assign(Ident("foo"), expr annotated Number)
    builtin() ~> stmt ~> annotated
  }

  test("statement.assignment.var") {
    val expr = Var(Ident("bar"))
    val stmt = Assign(Ident("foo"), expr)
    val annotated = Assign(Ident("foo"), expr annotated Number)
    builtin(("bar", 0) -> Number) ~> stmt ~> annotated
  }

  test("statement.assignment.chain") {
    val comp = Application(Ident("source"), Seq())
    val expr = Chain(Seq(comp))
    val stmt = Assign(Ident("foo"), expr)
    val annotated = Assign(Ident("foo"), Chain(Seq(comp annotated Source)) annotated Source)
    builtin(("source", 0) -> Source) ~> stmt ~> annotated
  }

  test("statement.assignment.parallel") {
    val source1 = Application(Ident("source1"), Seq())
    val source2 = Application(Ident("source2"), Seq())
    val expr = Parallel(Seq(source1, source2))
    val stmt = Assign(Ident("foo"), expr)
    val annotated = Assign(Ident("foo"), Parallel(Seq(
      source1 annotated Source, source2 annotated Source)) annotated Component(0, 2))
    builtin(("source1", 0) -> Source, ("source2", 0) -> Source) ~> stmt ~> annotated
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
    builtin(("source", 1) -> Source) ~> stmt ~> annotated
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
    builtin() ~> stmts ~> annotated
  }

  test("statement.assignment.overload.builtin") {
    builtin(("foo", 0) -> Source) ~> Assignment("foo", Seq("param"), Num(1)) ~>
      Assignment("foo", Seq("param"), Num(1) annotated Number)
  }

  test("statement.assignment.overload.builtin.bothVisible") {
    builtin(("foo", 0) -> Source) ~>
    Seq(
      Assignment("foo", Seq("param"), Var("param")),
      Assignment("bar1", Seq(), Var("foo")),
      Assignment("bar2", Seq(), Application("foo", Seq(Num(1))))
    ) ~> Seq(
      Assignment("foo", Seq("param"), Var("param") annotated Number),
      Assignment("bar1", Seq(), Var("foo") annotated Source),
      Assignment("bar2", Seq(), Application("foo", Seq(Num(1) annotated Number)) annotated Number)
    ) ordered (
      ("foo", 1) before ("bar2", 0)
    )
  }

  test("statement.assignment.overload.userDefined") {
    builtin() ~> Seq(
      Assignment("foo", Seq(), Num(1)),
      Assignment("foo", Seq("param"), Var("param"))
    ) ~> Seq(
      Assignment("foo", Seq(), Num(1) annotated Number),
      Assignment("foo", Seq("param"), Var("param") annotated Number)
    )
  }

  test("statement.assignment.overload.userDefined.bothVisible") {
    builtin() ~> Seq(
      Assignment("foo", Seq(), Num(1)),
      Assignment("foo", Seq("param"), Var("param")),
      Assignment("bar1", Seq(), Var("foo")),
      Assignment("bar2", Seq(), Application("foo", Seq(Num(1))))
    ) ~> Seq(
      Assignment("foo", Seq(), Num(1) annotated Number),
      Assignment("foo", Seq("param"), Var("param") annotated Number),
      Assignment("bar1", Seq(), Var("foo") annotated Number),
      Assignment("bar2", Seq(), Application("foo", Seq(Num(1) annotated Number)) annotated Number)
    ) ordered (
      ("foo", 0) before ("bar1", 0),
      ("foo", 1) before ("bar2", 0)
    )
  }

  test("statement.assignment.reordering.two") {
    builtin() ~> Seq(
      Assignment("foo", Seq(), Var("bar")),
      Assignment("bar", Seq(), Num(1))
    ) ~> Seq(
      Assignment("bar", Seq(), Num(1) annotated Number),
      Assignment("foo", Seq(), Var("bar") annotated Number)
    ) ordered (
      ("bar", 0) before ("foo", 0)
    )
  }

  test("statement.assignment.reordering.nested.param") {
    builtin(("s", 1) -> Number) ~>
    Seq(
      Assignment("three", Seq(), Application("s", Seq(Var("two")))),
      Assignment("two", Seq(), Num(2))
    ) ~>
    Seq(
      Assignment("two", Seq(), Num(2) annotated Number),
      Assignment("three", Seq(), Application("s", Seq(Var("two") annotated Number)) annotated Number)
    ) ordered (
      ("two", 0) before ("three", 0)
    )
  }

  test("statement.assignment.reordering.nested.binOp") {
    builtin() ~> Seq(
      Assignment("a", Seq(), BinOp(Num(2), Plus, Var("b"))),
      Assignment("b", Seq(), Num(1))
    ) ~> Seq(
      Assignment("b", Seq(), Num(1) annotated Number),
      Assignment("a", Seq(), BinOp(
        Num(2) annotated Number,
        Plus,
        Var("b") annotated Number) annotated Number)
    ) ordered (
      ("b", 0) before ("a", 0)
    )
  }

  test("statement.assignment.reordering.nested.chain") {
    builtin(("source", 0) -> Source) ~> Seq(
      Assignment("a", Seq(), Chain(Seq(Var("b")))),
      Assignment("b", Seq(), Var("source"))
    ) ~> Seq(
      Assignment("b", Seq(), Var("source") annotated Source),
      Assignment("a", Seq(), Chain(Seq(Var("b") annotated Source)) annotated Source)
    ) ordered (
      ("b", 0) before ("a", 0)
    )
  }

  test("statement.assignment.reordering.nested.parallel") {
    builtin(("source", 0) -> Source) ~> Seq(
      Assignment("a", Seq(), Parallel(Seq(Var("b")))),
      Assignment("b", Seq(), Var("source"))
    ) ~> Seq(
      Assignment("b", Seq(), Var("source") annotated Source),
      Assignment("a", Seq(), Parallel(Seq(Var("b") annotated Source)) annotated Source)
    ) ordered (
      ("b", 0) before ("a", 0)
    )
  }

  test("statement.assignment.reordering.nested.let") {
    builtin() ~> Seq(
      Assignment("a", Seq(), Let(Seq(Assignment("b", Seq(), Var("c"))), Var("b"))),
      Assignment("c", Seq(), Num(1))
    ) ~> Seq(
      Assignment("c", Seq(), Num(1) annotated Number),
        Assignment("a", Seq(), Let(
          Seq(
            Assignment("b", Seq(), Var("c") annotated Number)
          ),
          Var("b") annotated Number
        ) annotated Number
      )
    ) ordered (
      ("c", 0) before ("a", 0)
    )
  }

  test("statement.assignment.reordering.many") {
    builtin() ~> Seq(
      Assignment("f", Seq(), Application("c", Seq(Var("e")))),
      Assignment("d", Seq(), Var("a")),
      Assignment("e", Seq(), Var("b")),
      Assignment("a", Seq(), Num(1)),
      Assignment("b", Seq(), Num(2)),
      Assignment("c", Seq("param"), Var("param"))
    ) ~> Seq(
      Assignment("f", Seq(), Application("c", Seq(Var("e") annotated Number)) annotated Number),
      Assignment("d", Seq(), Var("a") annotated Number),
      Assignment("e", Seq(), Var("b") annotated Number),
      Assignment("a", Seq(), Num(1) annotated Number),
      Assignment("b", Seq(), Num(2) annotated Number),
      Assignment("c", Seq("param"), Var("param") annotated Number)
    ) ordered (
      ("a", 0) before ("d", 0),
      ("b", 0) before ("e", 0),
      ("c", 1) before ("f", 0),
      ("e", 0) before ("f", 0)
    )
  }

  /**
   * The bringToFront pitfall occurs when we naively bring all dependencies of an assignment to the
   * front of the list when we check that assignment, regardless of whether they have already been
   * checked. This can cause assignments which have already been moved forward to be reordered
   * within the front part of the list, which can break interdependencies among those assignments.
   * For example,
   *     c = a
   *     b = c
   *     a = 1
   * would, after typechecking c, be rearranged to
   *     a = 1
   *     c = a
   *     b = c
   * Then, when we typecheck b, since it references c, we would move c to the front, resulting in
   *     c = a
   *     a = 1
   *     b = c
   * This causes a dependency failure between c and a. We should move elements not to the front of
   * the list but only to the front of the unchecked elements.
   */
  test("statement.assignment.reordering.pitfall.bringToFront") {
    builtin() ~> Seq(
      Assignment("c", Seq(), Var("a")),
      Assignment("b", Seq(), Var("c")),
      Assignment("a", Seq(), Num(1))
    ) ~> Seq(
      Assignment("a", Seq(), Num(1) annotated Number),
      Assignment("c", Seq(), Var("a") annotated Number),
      Assignment("b", Seq(), Var("c") annotated Number)
    ) ordered (
      ("a", 0) before ("c", 0),
      ("c", 0) before ("b", 0)
    )
  }

  /**
   * The moveBackwards pitfall occurs when we naively move an element to the front of the unchecked
   * elements whenever it is referenced. If we do so and that element had already been checked, we
   * may actually be moving it backwards in the list, which can break dependencies among the front
   * part of the list (the already-checked section). For example,
   *     a = b
   *     b = 1
   *     c = b
   * would, after typechecking a, be rearranged to
   *     b = 1
   *     a = b
   *     c = b
   * Then, when we typecheck c, since it references b, we would move b backwards, resulting in
   *     a = b
   *     b = 1
   *     c = b
   * which causes a dependency failure between a and b. We sould only reorder elements that have
   * not already been typechecked and moved.
   */
  test("statement.assignment.reordering.pitfall.moveBackwards") {
    builtin() ~> Seq(
      Assignment("a", Seq(), Var("b")),
      Assignment("b", Seq(), Num(1)),
      Assignment("c", Seq(), Var("b"))
    ) ~> Seq(
      Assignment("b", Seq(), Num(1) annotated Number),
      Assignment("a", Seq(), Var("b") annotated Number),
      Assignment("c", Seq(), Var("b") annotated Number)
    ) ordered (
      ("b", 0) before ("a", 0),
      ("b", 0) before ("c", 0)
    )
  }

  test("statement.assignment.invalid.repeatedArg") {
    val name = Ident("foo")
    val param = Ident("param")
    val definition = Var(Ident("definition"))

    builtin(("definition", 0) -> Number) ~> Assignment(name, Seq(param, param), definition) ~/> statement
  }

  test("statement.assignment.invalid.unknownVar") {
    builtin() ~> Assign(Ident("foo"), Var(Ident("bar"))) ~/> statement
  }

  test("statement.assignment.invalid.reassignment.builtin.sameType") {
    builtin(("foo", 0) -> Number) ~> Assign(Ident("foo"), Num(42)) ~/> statement
  }

  test("statement.assignment.invalid.reassignment.builtin.differentType") {
    builtin(("foo", 0) -> Source) ~> Assign(Ident("foo"), Num(42)) ~/> statement
  }

  test("statement.assignment.invalid.reassignment.userDefined.sameType") {
    builtin() ~> Seq(
      Assign("foo", Num(42)),
      Assign("foo", Num(43))
    ) ~/> program
  }

  test("statement.assignment.invalid.reassignment.userDefined.differentType") {
    builtin(("source", 0) -> Source) ~> Seq(
      Assign("foo", Var("source")),
      Assign("foo", Num(42))
    ) ~/> program
  }

  test("statement.assignment.invalid.recursive.known") {
    builtin(("foo", 0) -> Number) ~> Assign(Ident("foo"), Var(Ident("foo"))) ~/> statement
  }

  test("statement.assignment.invalid.recursive.unknown") {
    builtin() ~> Assign(Ident("foo"), Var(Ident("foo"))) ~/> statement
  }

  test("statement.assignment.invalid.recursive.mutual") {
    builtin() ~> Seq(
      Assign(Ident("foo"), Var("bar")),
      Assign(Ident("bar"), Var("foo"))
    ) ~/> program
  }

  test("statement.instrument.var") {
    val channel = Num(1)
    val body = Var(Ident("source"))
    val instr = Instrument(Seq(channel), body)
    val annotated = Instrument(Seq(channel annotated Number), body annotated Source)
    builtin(("source", 0) -> Source) ~> instr ~> annotated
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

    builtin(("source", 0) -> Source, ("effect", 0) -> Effect) ~> instr ~> annotated
  }

  test("statement.instrument.varChannel") {
    val channel = Var(Ident("channel"))
    val body = Var(Ident("source"))
    val instr = Instrument(Seq(channel), body)
    val annotated = Instrument(Seq(channel annotated Number), body annotated Source)
    builtin(("source", 0) -> Source, ("channel", 0) -> Number) ~> instr ~> annotated
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

    builtin(("source", 1) -> Source) ~> instr ~> annotated
  }

  test("statement.instrument.midiParams.freq") {
    testMidiParam("freq")
  }

  test("statement.instrument.midiParams.amp") {
    testMidiParam("amp")
  }

  test("statement.instrument.sends.var") {
    val channel = Num(1)
    val body = Var(Ident("source"))
    val sends = Var(Ident("effect"))
    val instr = Instrument(Seq(channel), body, Some(sends))
    val annotated = Instrument(
      Seq(channel annotated Number), body annotated Source, Some(sends annotated Effect))
    builtin(("source", 0) -> Source, ("effect", 0) -> Effect) ~> instr ~> annotated
  }

  test("statement.instrument.sends.chain") {
    val channel = Num(1)
    val body = Var(Ident("source"))
    val sends = Chain(Seq(Var("effect1"), Var("effect2")))
    val sendsAnnotated = Chain(Seq(
      Var("effect1") annotated Effect, Var("effect2") annotated Effect)) annotated Effect
    val instr = Instrument(Seq(channel), body, Some(sends))
    val annotated = Instrument(
      Seq(channel annotated Number), body annotated Source, Some(sendsAnnotated))
    builtin(("source", 0) -> Source, ("effect1", 0) -> Effect, ("effect2", 0) -> Effect) ~> instr ~> annotated
  }

  test("statement.instrument.sends.invalid.number") {
    val channel = Num(1)
    val body = Var(Ident("source"))
    val sends = Num(2)
    val instr = Instrument(Seq(channel), body, Some(sends))
    builtin(("source", 0) -> Source) ~> instr ~/> statement
  }

  test("statement.instrument.sends.invalid.source") {
    val channel = Num(1)
    val body = Var(Ident("source1"))
    val sends = Var(Ident("source2"))
    val instr = Instrument(Seq(channel), body, Some(sends))
    builtin(("source1", 0) -> Source, ("source2", 0) -> Source) ~> instr ~/> statement
  }

  test("statement.instrument.invalid.illtypedChannel") {
    val channel = Var(Ident("source"))
    val body = Var(Ident("source"))
    val instr = Instrument(Seq(channel), body)
    builtin(("source", 0) -> Source) ~> instr ~/> statement
  }

  test("statement.instrument.invalid.number") {
    val channel = Num(1)
    val body = Num(2)
    val instr = Instrument(Seq(channel), body)
    builtin() ~> instr ~/> statement
  }

  test("statement.instrument.invalid.effectVar") {
    val channel = Num(1)
    val body = Var(Ident("effect"))
    val instr = Instrument(Seq(channel), body)
    builtin(("effect", 0) -> Effect) ~> instr ~/> statement
  }

  test("statement.instrument.invalid.effectChain") {
    val channel = Num(1)
    val body = Chain(Seq(Application(Ident("effect"), Seq())))
    val instr = Instrument(Seq(channel), body)
    builtin(("effect", 0) -> Effect) ~> instr ~/> statement
  }

  test("statement.instrument.invalid.unknownVar") {
    val channel = Num(1)
    val body = Var(Ident("foo"))
    val instr = Instrument(Seq(channel), body)
    builtin() ~> instr ~/> statement
  }

  test("statement.instrument.invalid.manyOuts") {
    val channel = Num(1)
    val body = Parallel(Seq(Var("source1"), Var("source2")))
    val instr = Instrument(Seq(channel), body)
    builtin(("source1", 0) -> Source, ("source2", 0) -> Source) ~> instr ~/> statement
  }

  test("statement.instrument.invalid.oneInOneOut") {
    val channel = Num(1)
    val body = Parallel(Seq(Var("effect1")))
    val instr = Instrument(Seq(channel), body)
    builtin(("effect1", 0) -> Effect) ~> instr ~/> statement
  }

  test("statement.instrument.invalid.oneInTwoOut") {
    val channel = Num(1)
    val body = Parallel(Seq(Var("source1"), Var("effect1")))
    val instr = Instrument(Seq(channel), body)
    builtin(("source1", 0) -> Source, ("effect1", 0) -> Effect) ~> instr ~/> statement
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Program tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  test("program.valid") {
    builtin(("fm", 3) -> Source, ("compress", 2) -> Effect) ~>
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
    ) ordered (
      ("bar", 0) before ("effect", 0)
    )
  }

  test("program.invalid.firstLine") {
    builtin(("fm", 3) -> Source, ("compress", 2) -> Effect) ~>
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
    builtin(("fm", 3) -> Source, ("compress", 2) -> Effect) ~>
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
    builtin(("fm", 3) -> Source, ("compress", 2) -> Effect) ~>
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
