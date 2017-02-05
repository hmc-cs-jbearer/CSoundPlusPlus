package soundwave

import scala.collection.immutable.{HashMap,HashSet}
import scala.util.parsing.input.Positional

import absyn._
import AbsynSugar._
import common.HashList

object SwTypeChecker {

  // Map from identifiers to types
  type TypeMap = HashMap[(Ident, Int), SwType]

  // Map from identifiers to definitions
  type AssignmentMap = HashMap[(Ident, Int), Assignment]

  case class Context(
    // Assignments which have yet to be typechecked.
    assignmentsToCheck: AssignmentMap = new AssignmentMap(),

    // This is ordered, because the order in which we translate assignments matters. Because it is
    // ordered, we will not lookup types from here to satisfy dependencies. Instead...
    checkedAssignments: Seq[Assignment] = Seq(),

    // ... we look up dependencies from here, which contains just the type information.
    checkedTypes: TypeMap = new TypeMap(),

    // Instrument definitions which need to be typechecked.
    instrumentsToCheck: Seq[Instrument] = Seq(),

    checkedInstruments: Seq[Instrument] = Seq(),

    // Built-in csound code which will be linked with this program. Since these are built-in, there
    // are no definitions available, just the types.
    primitives: TypeMap = new TypeMap(),

    // We don't need the definition of locals when we look up a local, so we just store the type
    locals: TypeMap = new TypeMap()
  )

  /**
   * Typecheck the given program. Typechecking is performed in an environment where built-in
   * components are already mapped to their proper type.
   */
  def apply(ast: Seq[Statement]): Either[SwTypeError, Seq[Statement]] = apply(
    new TypeMap() + (
      // (name, arity) -> resultType
      (Ident("foscil"), 4)               -> Source,
      (Ident("sine"), 2)                 -> Source,
      (Ident("pulse"), 2)                -> Source,
      (Ident("sidechain_compress"), 5)   -> Component(2, 1),
      (Ident("compress"), 5)             -> Effect,
      (Ident("adsr"), 4)                 -> Effect,
      (Ident("average"), 0)              -> Component(4, 1),
      (Ident("delay"), 2)                -> Component(1, 1),
      (Ident("reverb"), 1)               -> Component(1, 1),
      (Ident("scale"), 1)                -> Component(1, 1),
      (Ident("transpose"), 1)            -> Component(1, 1),
      (Ident("filt"), 4)                 -> Component(1, 1),
      (Ident("sqrt"), 1)                 -> Number,
      (Ident("lopass_sweep"), 6)         -> Component(1, 1),
      (Ident("sum"), 0)                  -> Component(4, 1)
    ),
    ast
  )

  // Used for unit tests that want to specify their own set of built-in bindings
  def apply(builtins: TypeMap, ast: Seq[Statement]): Either[SwTypeError, Seq[Statement]] = {
    try {
      val context = scan(Context(primitives = builtins), ast)
      val context2 = annotateAssignments(context)
      val context3 = annotateInstruments(context2)
      Right(unscan(context3))
    } catch {
      case e: SwTypeError => Left(e)
    }
  }

  // Build a map of all assignments in the program
  def scan(context: Context, stmts: Seq[Statement]): Context =
    if (stmts.isEmpty)
      context
    else {
      val context2 = scan(context, stmts.tail)
      stmts.head match {
        case a @ Assignment(id, params, _) =>
          if (context2.assignmentsToCheck contains (id, params.size))
            throw new SwTypeError(a.loc, s"Redefinition of $id(${params.size}). " +
              s"First declared here: ${context2.assignmentsToCheck((id, params.size)).loc}.")
          else if (context2.primitives contains (id, params.size))
            throw new SwTypeError(a.loc, s"Redefinition of built-in $id(${params.size})")
          else
            context2.copy(
              assignmentsToCheck = context2.assignmentsToCheck + ((id, params.size) -> a))
        case i: Instrument => context2.copy(instrumentsToCheck = i +: context2.instrumentsToCheck)
      }
    }

  def unscan(context: Context): Seq[Statement] =
    context.checkedAssignments ++ context.checkedInstruments

  def annotateAssignments(context: Context): Context =
    if (context.assignmentsToCheck.isEmpty)
      context
    else {
      val a = context.assignmentsToCheck.head._2
      val context2 = context.copy(assignmentsToCheck = context.assignmentsToCheck.tail)
      annotateAssignments(annotateAssignment(context2, a))
    }

  def annotateAssignment(context: Context, current: Assignment): Context = {
    val Assignment(id, params, expr) = current

    // Bring the parameters into scope
    val locals = params map ((_, 0)) // Each param is a 0-ary function resulting in a number
    assertDistinct(locals)
    val localContext = addLocals(context, locals map (_ -> Number))

    val (localContext2, annotated) = annotateExpr(localContext, expr)

    localContext2.copy(
      // Revert the locals back to the state they were in when we got them
      locals = context.locals,
      // Remember the type of the assignment we just checked
      checkedTypes = localContext2.checkedTypes + ((id, params.length) -> typeOf(annotated)),
      // Add the annotated assignment to the end of the output list
      checkedAssignments = localContext2.checkedAssignments :+ current.copy(definition = annotated)
    )
  }

  def annotateInstruments(context: Context): Context =
    if (context.instrumentsToCheck.isEmpty)
      context
    else {
      val i = context.instrumentsToCheck.head
      val context2 = context.copy(instrumentsToCheck = context.instrumentsToCheck.tail)
      annotateInstruments(annotateInstrument(context2, i))
    }

  def annotateInstrument(context: Context, i: Instrument): Context = {
    val Instrument(channels, definition, sends) = i

    def assertChannels(context: Context, channels: Seq[Expr]): (Context, Seq[Expr]) =
      if (channels.isEmpty)
        (context, Seq())
      else {
        val (context2, head) = assertExpr(context, channels.head, Number)
        val (context3, tail) = assertChannels(context2, channels.tail)
        (context3, head +: tail)
      }

    val (context2, annotatedChannels) = assertChannels(context, channels)

    // Bring MIDI parameters into scope
    val localContext = addLocals(context, Seq((Ident("amp"), 0) -> Number, (Ident("freq"), 0) -> Number))

    val (localContext2, annotatedDefinition) = assertExpr(localContext, definition, Component(0, 1))
    val (localContext3, annotatedSends) = sends match {
      case None => (localContext2, None)
      case Some(body) => {
        val (localContext3, annotatedSends) = assertExpr(localContext2, body, Component(1, 1))
        (localContext3, Some(annotatedSends))
      }
    }

    val annotated = i.copy(
      channels = annotatedChannels,
      definition = annotatedDefinition,
      sends = annotatedSends
    )

    localContext3.copy(
      // Restore locals
      locals = context.locals,
      // Save the new instrument
      checkedInstruments = localContext3.checkedInstruments :+ annotated
    )
  }

  def annotateExpr(context: Context, expr: Expr): (Context, Expr) = expr match {
    case n: Num => (context, n annotated Number)

    case BinOp(l, op, r, _) => {
      val (context2, annotatedLeft) = assertExpr(context, l, Number)
      val (context3, annotatedRight) = assertExpr(context2, r, Number)
      (context3, BinOp(annotatedLeft, op, annotatedRight) annotated Number)
    }

    case Application(id, args, _) => {
      def assertArgs(context: Context, args: Seq[Expr]): (Context, Seq[Expr]) =
        if (args.isEmpty)
          (context, Seq())
        else {
          val (context2, head) = assertExpr(context, args.head, Number)
          val (context3, tail) = assertArgs(context2, args.tail)
          (context3, head +: tail)
        }
      val (context2, annotatedArgs) = assertArgs(context, args)
      val (context3, ty) = lookupVar(context2, id, args.length)
      (context3, Application(id, annotatedArgs) annotated ty)
    }

    case Chain(body, _) => {
      def assertComponents(context: Context, body: Seq[Expr], inArity: Int): (Context, Seq[Expr]) =
        if (body.isEmpty) {
          (context, Seq())
        } else {
          val (context2, head, headOutArity) = assertExpr(context, body.head,
            s"component with $inArity inputs",
            { case Component(in, out) if in == inArity => out })

          val (context3, tail) = assertComponents(context2, body.tail, headOutArity)

          (context3, head +: tail)
        }

      if (body.isEmpty) {
        // An empty chain is the boring effect that passes its input through unchanged.
        (context, expr annotated Effect)
      } else {
        val (context2, head, (inArity, headOutArity)) =
          assertExpr(context, body.head, "component", { case Component(in, out) => (in, out) })

        val (context3, tail) = assertComponents(context2, body.tail, headOutArity)

        val (context4, _, outArity) = assertExpr(context3, (head +: tail).last, "component",
                                       { case Component(_, out) => out })

        (context4, Chain(head +: tail) annotated Component(inArity, outArity))
      }
    }

    case Parallel(body, _) => {
      // Annotate the inputs, and compute the arity
      def assertComponents(context: Context, components: Seq[Expr]):
        (Context, Seq[Expr], Seq[(Int, Int)]) =
        if (components.isEmpty)
          (context, Seq(), Seq())
        else {
          val (context2, head, arity) = assertExpr(context, components.head, "component",
            { case Component(in, out) => (in, out) })
          val (context3, tail, arities) = assertComponents(context2, components.tail)
          (context3, head +: tail, arity +: arities)
        }

      val (context2, annotatedInputs, arities) = assertComponents(context, body)

      val (inArities, outArities) = arities.unzip

      val (inArity, outArity) = if (body.isEmpty) {
        // An empty parallel block is treated the same as an empty chain: a trivial effect
        (1, 1)
      } else {
        (inArities.sum, outArities.sum)
      }

      (context2, Parallel(annotatedInputs) annotated Component(inArity, outArity))
    }

    case Let(bindings, body, _) => {
      def annotateBindings(context: Context, bindings: Seq[Assignment]): (Context, Seq[Assignment]) =
        if (bindings.isEmpty) {
          (context, Seq())
        } else {
          val (context2, annotatedHead) = annotateBinding(context, bindings.head)
          val (context3, annotatedTail) = annotateBindings(context2, bindings.tail)
          (context3, annotatedHead +: annotatedTail)
        }

      def annotateBinding(context: Context, binding: Assignment): (Context, Assignment) = {
          val Assignment(id, params, expr) = binding

          // Add the parameters to the local scope
          val locals = params map ((_, 0)) // Each param is a 0-ary function resulting in a number
          assertDistinct(locals)
          val localContext = addLocals(context, locals map ((_ -> Number)))

          val (localContext2, annotated) = annotateExpr(localContext, expr)

          // Restore locals
          val context2 = localContext2.copy(locals = context.locals)

          (addLocal(context2, id, params.length, typeOf(annotated)), binding.copy(definition = annotated))
        }

      // Bindings in a let statement can shadow variables from an outerscope, but must be unique
      // amongst each other
      assertDistinct(bindings collect {
        case Assignment(id, params, _) => (id, params.length)
      })
      val (localContext, annotatedBindings) = annotateBindings(context, bindings)
      val (localContext2, annotatedBody) = annotateExpr(localContext, body)
      // Restore the locals
      val context2 = localContext2.copy(locals = context.locals)

      (context2, Let(annotatedBindings, annotatedBody) annotated typeOf(annotatedBody))
    }
  }

  def assertExpr(context: Context, expr: Expr, expected: SwType): (Context, Expr) = {
    val (context2, annotated, _) = assertExpr(context, expr, expected.toString,
      { case ty if ty == expected => () })
    (context2, annotated)
  }

  def assertExpr[T](context: Context, expr: Expr, expected: String, f: PartialFunction[SwType, T])
    : (Context, Expr, T) =
  {
    val (context2, annotated) = annotateExpr(context, expr)
    val ty = typeOf(annotated)
    if (f isDefinedAt ty) {
      (context2, annotated, f(ty))
    } else {
      throw new SwTypeError(
        expr.loc, s"Expected ${expected} but found ${ty}.")
    }
  }

  def assertDistinct(toCheck: Seq[(Ident, Int)], checked: Set[(Ident, Int)] = Set()): Unit =
    if (!toCheck.isEmpty) {
      if (checked contains toCheck.head) {
        val id = toCheck.head._1
        throw new SwTypeError(id.loc, s"Redefinition of ${id.name}.")
      } else {
        assertDistinct(toCheck.tail, checked + toCheck.head)
      }
    }

  def lookupVar(context: Context, id: Ident, arity: Int): (Context, SwType) =
    // Check locals first, since locals shadow globals
    if (context.locals contains (id, arity))
      (context, context.locals((id, arity)))
    // Then check primitives
    else if (context.primitives contains (id, arity))
      (context, context.primitives((id, arity)))
    // Check annotated globals
    else if (context.checkedTypes contains (id, arity))
      (context, context.checkedTypes((id, arity)))
    // We have a global dependency which has not yet been typechecked
    else if (context.assignmentsToCheck contains (id, arity)) {
      val a = context.assignmentsToCheck((id, arity))
      val context2 = context.copy(assignmentsToCheck = context.assignmentsToCheck - ((id, arity)))
      val context3 = annotateAssignment(context2, a)
      (context3, context3.checkedTypes((id, arity)))
    } else {
      throw new SwTypeError(id.loc, s"Unknown function $id($arity).")
    }

  def addLocal(context: Context, id: Ident, arity: Int, ty: SwType): Context =
    context.copy(locals = context.locals + ((id, arity) -> ty))

  def addLocals(context: Context, locals: Seq[((Ident, Int), SwType)]): Context =
    if (locals.isEmpty)
      context
    else {
      val head = context.copy(locals = context.locals + locals.head)
      addLocals(head, locals.tail)
    }

  def typeOf[T <: TypeAnnotation with SwPositional](elem: T) = elem.ty match {
    case Some(ty) => ty
    case None => throw new SwTypeError(
      elem.loc, s"Unable to deduce type of $elem.")
  }
}
