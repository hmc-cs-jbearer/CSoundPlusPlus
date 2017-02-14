package soundwave

import scala.collection.Map
import scala.reflect._
import scala.runtime._

import absyn._
import AbsynSugar._

object SwTypeChecker {

  // Map from identifiers to types
  type TypeMap = Map[(Ident, Int), SwType]
  val TypeMap = Map

  // Map from identifiers to definitions
  type AssignmentMap = Map[(Ident, Int), Assignment]
  val AssignmentMap = Map

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Monadic typecheckers encapsulating the state
  //////////////////////////////////////////////////////////////////////////////////////////////////

  // A computation waiting for a context in which to be performed
  abstract class TypecheckerBase[Result] {
    // Perform the computation
    def apply(context: Typechecker.Context): Either[SwError, (Typechecker.Context, Result)]
    def apply(): Either[SwError, Result] = apply(Typechecker.Context()) match {
      case Left(err)                => Left(err)
      case Right((context, result)) => Right(result)
    }

    def filter(t: Result => Boolean): Typechecker[Result]

    // Monadic bind
    def map[T](f: Result => T): Typechecker[T]
    def flatMap[T](f: Result => Typechecker[T]): Typechecker[T]
  }

  case class Typechecker[Result](
    compute: Typechecker.Context => Either[SwError, (Typechecker.Context, Result)])
    extends TypecheckerBase[Result]
  {
    def apply(context: Typechecker.Context) = compute(context)

    object FilterError extends SwError {
      override def toString = "filter returned false"
    }

    def filter(p: Result => Boolean) = Typechecker { context => this(context) match {
        case Left(err)                                  => Left(err)
        case r @ Right((context2, result)) if p(result) => r
        case _                                          => Left(FilterError)
      }
    }

    def withFilter(p: Result => Boolean) = filter(p)

    def map[T](f: Result => T) = Typechecker { context => this(context) match {
        case Left(err)                  => Left(err)
        case Right((context2, result))  => Right((context2, f(result)))
      }
    }

    def flatMap[T](f: Result => Typechecker[T]) = Typechecker { context => this(context) match {
        case Left(err)                  => Left(err)
        case Right((context2, result))  => f(result)(context2)
      }
    }
  }

  object Typechecker {
    type Context = Map[String, Any]
    val Context = Map

    // Monadic identity
    def apply[Result](r: Result): Typechecker[Result] =
      Typechecker { context => Right((context, r)) }
  }

  def emit[T](result: T): Typechecker[T] = Typechecker(result)
  def fail[T](error: SwError): Typechecker[T] = Typechecker { context => Left(error) }


  val boxedClasses = Map[Class[_], Class[_]](
    classOf[Double] -> classOf[java.lang.Double],
    classOf[Float]  -> classOf[java.lang.Float],
    classOf[Long]   -> classOf[java.lang.Long],
    classOf[Int]    -> classOf[java.lang.Integer],
    classOf[Char]   -> classOf[java.lang.Character],
    classOf[Short]  -> classOf[java.lang.Short],
    classOf[Byte]   -> classOf[java.lang.Byte],
    classOf[Unit]   -> classOf[BoxedUnit],
    classOf[Boolean]-> classOf[java.lang.Boolean]
  )
  def boxed(c: Class[_]) = boxedClasses.getOrElse(c, c)
  def isInstance[T: ClassTag](that: Any) = boxed(classTag[T].runtimeClass).isInstance(that)
  def nameOfType[T: ClassTag] = classTag[T].runtimeClass.toString

  // Get the context associated with key. Error if key is not present or if the value associated
  // with key is not an instance of T.
  def getContext[T: ClassTag](key: String): Typechecker[T] = Typechecker { context =>
    if (context contains key)
      if (isInstance[T](context(key)))
        Right((context, context(key).asInstanceOf[T]))
      else
        Left(SwInternalError(s"Invalid cast: ${context(key)} is not an instance of ${nameOfType[T]}"))
    else
      Left(SwInternalError(s"Invalid context lookup: unknown key $key"))
  }

  // Add a new key-value mapping to the context. Error if key already exists.
  def putContext(key: String, value: Any): Typechecker[Unit] = Typechecker { context =>
    if (context contains key)
      Left(SwInternalError(s"Overwriting key $key"))
    else
      Right((context + (key -> value), ()))
  }

  // Set the value associted with key and return the old value. Error if key is not present, or if
  // the value stored at key is not an instance of T
  def updateContext[T: ClassTag](key: String, value: T): Typechecker[T] = Typechecker { context =>
    if (context contains key)
      if (isInstance[T](context(key))) {
        val oldValue = context(key).asInstanceOf[T]
        Right((context + (key -> value), oldValue))
      } else {
        Left(SwInternalError(s"Invalid cast: ${context(key)} is not an instance of ${nameOfType[T]}"))
      }
    else
      Left(SwInternalError(s"Updating unkown key: $key"))
  }

  // Remove a key-value mapping from the context and return its previous value. Error if key is not
  // present, or if the value stored at key is not an instance of T
  def removeContext[T: ClassTag](key: String): Typechecker[T] = Typechecker { context =>
    if (context contains key)
      if (isInstance[T](context(key))) {
        val oldValue = context(key).asInstanceOf[T]
        Right((context - key, oldValue))
      } else {
        Left(SwInternalError(s"Invalid cast: ${context(key)} is not an instance of ${nameOfType[T]}"))
      }
    else
      Left(SwInternalError(s"Removing unknown key: $key"))
  }

  def printContext: Typechecker[Unit] = Typechecker { context =>
    println(context)
    Right(context, ())
  }

  // Map a typechecker over a Seq, and then turn the Seq[Typechecker[T]] into a Typechecker[Seq[T]]
  def forEach[T, U](elems: Seq[T])(f: T => Typechecker[U]): Typechecker[Seq[U]] =
    if (elems.isEmpty)
      Typechecker(Seq())
    else for {
      head <- f(elems.head)
      tail <- forEach(elems.tail)(f)
    } yield head +: tail

  def initContext(
    // Assignments which have yet to be typechecked.
    assignmentsToCheck: AssignmentMap = AssignmentMap(),

    // This is ordered, because the order in which we translate assignments matters. Because it is
    // ordered, we will not lookup types from here to satisfy dependencies. Instead...
    checkedAssignments: Seq[Assignment] = Seq(),

    // ... we look up dependencies from here, which contains just the type information.
    checkedTypes: TypeMap = TypeMap(),

    // Instrument definitions which need to be typechecked.
    instrumentsToCheck: Seq[Instrument] = Seq(),
    checkedInstruments: Seq[Instrument] = Seq(),

    // Built-in csound code which will be linked with this program. Since these are built-in, there
    // are no definitions available, just the types.
    primitives: TypeMap = TypeMap(),

    // We don't need the definition of locals when we look up a local, so we just store the type
    locals: TypeMap = TypeMap()
  ) = for {
    _ <- putContext("assignmentsToCheck",   assignmentsToCheck)
    _ <- putContext("checkedAssignments",   checkedAssignments)
    _ <- putContext("checkedTypes",         checkedTypes)
    _ <- putContext("instrumentsToCheck",   instrumentsToCheck)
    _ <- putContext("checkedInstruments",   checkedInstruments)
    _ <- putContext("primitives",           primitives)
    _ <- putContext("locals",               locals)
  } yield ()

  def localScope[T](op: =>Typechecker[T]): Typechecker[T] = localScope(Seq())(op)

  def localScope[T](newLocals: Seq[((Ident, Int), SwType)])(op: =>Typechecker[T]): Typechecker[T] =
    for {
      oldLocals <- getContext[TypeMap]("locals")
      _         <- updateContext("locals", oldLocals ++ newLocals)
      result    <- op
      _         <- updateContext("locals", oldLocals)
    } yield result

  def localScope[T](local: ((Ident, Int), SwType), locals: ((Ident, Int), SwType)*)(
    op: =>Typechecker[T]): Typechecker[T] =
    localScope(local +: locals)(op)

  def addLocal(a: Assignment) = for {
    locals <- getContext[TypeMap]("locals")
    Assignment(id, params, expr) = a
    ty <- typeOf(expr)
    _ <- updateContext("locals", locals + ((id, params.length) -> ty))
  } yield ()

  /**
   * Typecheck the given program. Typechecking is performed in an environment where built-in
   * components are already mapped to their proper type.
   */
  def apply(ast: Seq[Statement]): Either[SwError, Seq[Statement]] =
    apply(SwPrimitives.types, ast)

  // Used for unit tests that want to specify their own set of built-in bindings
  def apply(builtins: TypeMap, ast: Seq[Statement]): Either[SwError, Seq[Statement]] = {
    val typechecker = for {
      _ <- initContext(primitives = builtins)
      _ <- scan(ast)
      _ <- annotateAssignments
      _ <- annotateInstruments
      result <- unscan
    } yield result

    typechecker()
  }

  // Build a map of all assignments in the program
  def scan(stmts: Seq[Statement]) = forEach(stmts) { stmt =>
    stmt match {
      case a @ Assignment(id, params, _) => for {
        primitives <- getContext[TypeMap]("primitives")
        assignmentsToCheck <- getContext[AssignmentMap]("assignmentsToCheck")
        _ <- if (assignmentsToCheck contains (id, params.size))
          fail(new SwTypeError(a.loc, s"Redefinition of $id(${params.size}). " +
              s"First declared here: ${assignmentsToCheck((id, params.size)).loc}."))
        else if (primitives contains (id, params.size))
          fail(new SwTypeError(a.loc, s"Redefinition of built-in $id(${params.size})"))
        else
          updateContext("assignmentsToCheck", assignmentsToCheck + ((id, params.size) -> a))
      } yield ()

      case i: Instrument => for {
        instrumentsToCheck <- getContext[Seq[Instrument]]("instrumentsToCheck")
        _ <- updateContext("instrumentsToCheck", instrumentsToCheck :+ i)
      } yield ()
    }
  }

  def unscan: Typechecker[Seq[Statement]] = for {
    checkedAssignments <- getContext[Seq[Statement]]("checkedAssignments")
    checkedInstruments <- getContext[Seq[Statement]]("checkedInstruments")
  } yield (checkedAssignments ++ checkedInstruments)

  def annotateAssignments: Typechecker[Any] = for {
    assignmentsToCheck <- getContext[AssignmentMap]("assignmentsToCheck")
    _ <- if (assignmentsToCheck.isEmpty) emit(()) else for {
      // Take this assignment off the todo list
      (key, a) <- emit(assignmentsToCheck.head)
      _ <- updateContext("assignmentsToCheck", assignmentsToCheck - key)
      _ <- annotateAssignment(a)
      _ <- annotateAssignments
    } yield ()
  } yield ()

  def annotateAssignment(a: Assignment): Typechecker[Unit] = for {
    Assignment(id, params, expr) <- emit(a)
    arity = params.length

    _ <- assertDistinct(params map ((_, 0)))

    // Annotate the assignment in a scope that includes the parameters
    annotated <- localScope(params map ((_, 0) -> Number)) { annotateExpr(expr) }

    // Remember the type of the assignment we just checked
    checkedTypes <- getContext[TypeMap]("checkedTypes")
    ty <- typeOf(annotated)
    _ <- updateContext("checkedTypes", checkedTypes + ((id, arity) -> ty))

    // Save the result
    checkedAssignments <- getContext[Seq[Assignment]]("checkedAssignments")
    _ <- updateContext("checkedAssignments", checkedAssignments :+ a.copy(definition = annotated))

  } yield ()

  def annotateInstruments: Typechecker[Any] = for {
    instrumentsToCheck <- getContext[Seq[Instrument]]("instrumentsToCheck")
    _ <- if (instrumentsToCheck.isEmpty) emit(()) else for {
      // Take this instrument off the todo list
      i <- emit(instrumentsToCheck.head)
      _ <- updateContext("instrumentsToCheck", instrumentsToCheck.tail)
      _ <- annotateInstrument(i)
      _ <- annotateInstruments
    } yield ()
  } yield ()

  def annotateInstrument(i: Instrument): Typechecker[Unit] = for {
    Instrument(channels, definition, sends) <- emit(i)

    annotatedChannels <- forEach(channels) { assertExpr(_, Number) }
    annotatedDefinition <-
      // Bring MIDI parameters into scope
      localScope((Ident("amp"), 0) -> Number, (Ident("freq"), 0) -> Number) {
        assertExpr(definition, Component(0, 1))
      }

    annotatedSends <- sends match {
      case Some(expr)   => for {
        annotated <- assertExpr(expr, Component(1, 1))
      } yield Some(annotated)
      case None         => emit(None)
    }

    // Save the result
    annotated = i.copy(
      channels = annotatedChannels,
      definition = annotatedDefinition,
      sends = annotatedSends
    )
    checkedInstruments <- getContext[Seq[Instrument]]("checkedInstruments")
    _ <- updateContext("checkedInstruments", checkedInstruments :+ annotated)
  } yield ()

  def annotateExpr(expr: Expr): Typechecker[Expr] = expr match {
    case n: Num => emit(n annotated Number)

    case b @ BinOp(l, _, r, _) => for {
      annotatedLeft <- assertExpr(l, Number)
      annotatedRight <- assertExpr(r, Number)
    } yield b.copy(
      left = annotatedLeft,
      right = annotatedRight
    ) annotated Number

    case a @ Application(id, args, _) => for {
      annotatedArgs <- forEach(args) { assertExpr(_, Number) }
      ty <- lookupVar(id, args.length)
    } yield a.copy(args = annotatedArgs) annotated ty

    case c @ Chain(body, _) =>
      if (body.isEmpty) {
        // An empty chain is the boring effect that passes its input through unchanged.
        emit(expr annotated Effect)
      } else for {
        (head, (inArity, headOutArity)) <- assertExpr(body.head, "component",
          { case Component(in, out) => (in, out) })

        // The inArity of the tail of the chain must be the outArity of the head
        _ <- putContext("inArity", headOutArity)

        // Check the tail, making sure each inter-component interface has the same arity
        tail <- forEach(body.tail) { comp =>
          for {
            // Make sure the component has the proper inArity, and get the outArity
            inArity <- getContext[Int]("inArity")
            (annotated, outArity) <- assertExpr(comp, s"component with $inArity, inputs",
              { case Component(in, out) if in == inArity => out })

            // The inArity of the next component is the outArity of this one
            _ <- updateContext("inArity", outArity)
          } yield annotated
        }

        // The last value of inArity is the outArity of the whole chain
        outArity <- removeContext[Int]("inArity")
      } yield c.copy(body = head +: tail) annotated Component(inArity, outArity)

    case p @ Parallel(body, _) =>
      if (body.isEmpty)
        // An empty parallel block is treated the same as an empty chain: a trivial effect
        emit(expr annotated Effect)
      else
        for {
          annotatedInputsAndArities <- forEach(body) {
            assertExpr(_, "component", { case Component(in, out) => (in, out) })
          }
          (annotatedInputs, arities) = annotatedInputsAndArities.unzip
          (inArities, outArities) = arities.unzip
          (inArity, outArity) = (inArities.sum, outArities.sum)
        } yield p.copy(body = annotatedInputs) annotated Component(inArity, outArity)

    case l @ Let(bindings, body, _) => localScope {
      for {
        // Bindings in a let statement can shadow variables from an outer scope, but must be
        // unique amongst each other
        _ <- assertDistinct(bindings collect {
          case Assignment(id, params, _) => (id, params.length)
        })

        // Annotate the bindings in the let
        annotatedBindings <- forEach(bindings) { binding =>
          for {
            Assignment(id, params, expr) <- emit(binding)
            _ <- assertDistinct(params map (_ -> 0))
            annotatedExpr <- localScope(params map ((_, 0) -> Number)) { annotateExpr(expr) }
            annotated = binding.copy(definition = annotatedExpr)
            _ <- addLocal(annotated)
          } yield annotated
        }

        // Annotate the body of the let
        annotatedBody <- annotateExpr(body)
        ty <- typeOf(annotatedBody)
      } yield l.copy(bindings = annotatedBindings, body = annotatedBody) annotated ty
    }

  }

  def assertExpr(expr: Expr, expected: SwType): Typechecker[Expr] = for {
    (annotated, _) <- assertExpr(expr, expected.toString, { case ty if ty == expected => () })
  } yield annotated

  def assertExpr[T](expr: Expr, expected: String, f: PartialFunction[SwType, T]):
    Typechecker[(Expr, T)] =
    for {
      annotated <- annotateExpr(expr)
      ty <- typeOf(annotated)
      result <-
        if (f isDefinedAt ty)
          emit((annotated, f(ty)))
        else
          fail(new SwTypeError(expr.loc, s"Expected ${expected} but found ${ty}."))
    } yield result

  def assertDistinct(toCheck: Seq[(Ident, Int)], checked: Set[(Ident, Int)] = Set()): Typechecker[Unit] =
    if (toCheck.isEmpty)
      emit(())
    else {
      if (checked contains toCheck.head) {
        val id = toCheck.head._1
        fail(new SwTypeError(id.loc, s"Redefinition of ${id.name}."))
      } else {
        assertDistinct(toCheck.tail, checked + toCheck.head)
      }
    }

  def lookupVar(id: Ident, arity: Int): Typechecker[SwType] = for {
    locals <- getContext[TypeMap]("locals")
    primitives <- getContext[TypeMap]("primitives")
    checkedTypes <- getContext[TypeMap]("checkedTypes")
    assignmentsToCheck <- getContext[AssignmentMap]("assignmentsToCheck")
    result <-
      // Check locals first, since locals shadow globals
      if (locals contains (id, arity))
        emit(locals((id, arity)))
      // The order in which we search from here on out doesn't matter, because we demand that
      // primitives and assignments are unique, and no shadowing occurs. Check primitives next.
      else if (primitives contains (id, arity))
        emit(primitives((id, arity)))
      // Check annotated globals
      else if (checkedTypes contains (id, arity))
        emit(checkedTypes((id, arity)))
      // We have a global dependency which has not yet been typechecked
      else if (assignmentsToCheck contains (id, arity))
        for {
          // Take the dependency off the todo list and check it
          _ <- updateContext("assignmentsToCheck", assignmentsToCheck - ((id, arity)))
          a = assignmentsToCheck((id, arity))
          _ <- annotateAssignment(a)

          // Now it is in the already checked list
          result <- lookupVar(id, arity)
        } yield result
      else
        fail(new SwTypeError(id.loc, s"Unknown function $id($arity)."))
  } yield result

  def typeOf[T <: TypeAnnotation with SwPositional](elem: T): Typechecker[SwType] = elem.ty match {
    case Some(ty) => emit(ty)
    case None     => fail(new SwTypeError(elem.loc, s"Unable to deduce type of $elem."))
  }
}
