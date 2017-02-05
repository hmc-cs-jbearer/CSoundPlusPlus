package soundwave

import scala.collection.immutable.{HashSet,HashMap}

import absyn._
import AbsynSugar._

/**
 * This object allocates various Csound identifiers to aid in the translation. For example, it
 * allocates an a-rate variable for every audio signal in the program. See the objects below for
 * details on the operations applied.
 */
object SwAllocator {
  def apply(stmts: Seq[Statement]): Either[SwCompileError, Seq[SwDag.Nodes.StmtNode]] =
    for {
      resolvedLets <- SwLetResolver(stmts).right
      dag <- SwDag(resolvedLets).right
    } yield dag
}

/**
 * The job of this object is to transform an abstract syntax tree by allocating unique global names
 * for every assignment made in a let statement, and to bring those assignments to the global scope
 * with the modified names, and to substitute the modified names into the body of the let expression
 * wherever the assignments are referenced.
 *
 * This is done because Csound will not let us declare "local" functions -- that would be opcodes
 * nested withing other opcodes or instruments. We can declare local variables (soundwave constants)
 * and hopefully we will eventually optimie this by "inlining" the variables and only "globalizing"
 * the functions/components.
 *
 * For now, though, it is much simpler to globalize everything.
 */
object SwLetResolver {

  case class Context(lets: Int)
  object Context {
    object Empty extends Context(0)
  }

  def apply(stmts: Seq[Statement]): Either[SwCompileError, Seq[Statement]] = {
    try {
      Right(resolveStmts(Context.Empty, stmts)._2)
    } catch {
      case e: SwCompileError => Left(e)
    }
  }

  def resolveStmts(context: Context, stmts: Seq[Statement]): (Context, Seq[Statement]) =
    if (stmts.isEmpty) {
      (context, Seq())
    } else {
      val (context2, headStatements) = resolveStmt(context, stmts.head)
      val (context3, tailStatements) = resolveStmts(context2, stmts.tail)
      (context3, headStatements ++ tailStatements)
    }

  def resolveStmt(context: Context, stmt: Statement): (Context, Seq[Statement]) = stmt match {
    case a @ Assignment(_, params, body) => {
      val (context2, resolvedBody, emittedStmts) = resolveExpr(context, body)

      // Explicitly capture params
      val (capturedStmts, capturedBody) = capture(params: _*)(emittedStmts, resolvedBody)

      val resolvedAsn = a copy (definition = capturedBody)
      (context2, capturedStmts :+ resolvedAsn)
    }

    case i @ Instrument(channels, body, sends) => {
      // Explicitly capture amp and freq
      val captureMidi = capture("amp", "freq") _

      val (context2, resolvedChannels, channelStmts) = resolveChannels(context, channels)
      val (context3, resolvedBody, bodyStmts) = resolveExpr(context2, body)
      val (capturedBodyStmts, capturedBody) = captureMidi(bodyStmts, resolvedBody)

      val (context4, capturedSends, capturedSendsStmts) = sends match {
        case Some(s) => {
          val (context4, resolvedSends, sendsStmts) = resolveExpr(context3, s)
          val (capturedSendsStmts, capturedSends) = captureMidi(sendsStmts, resolvedSends)
          (context4, Some(capturedSends), capturedSendsStmts)
        }
        case None    => (context3, None, Seq())
      }

      val resolvedInstr = i copy (
        channels = resolvedChannels,
        definition = capturedBody,
        sends = capturedSends
      )
      (context3, (channelStmts ++ capturedBodyStmts ++ capturedSendsStmts) :+ resolvedInstr)
    }
  }

  /**
   * Remove all let subexpressions in the given expression, replacing them with a sequence of global
   * assignments and references to those assignments
   */
  def resolveExpr(context: Context, expr: Expr): (Context, Expr, Seq[Statement]) = expr match {

    case Let(bindings, body, ty) => {
      val localNames = bindings map { case Assignment(name, _, _) => name }
      val (context2, globalNames) = letNames(context, localNames)
      val bindingsMap = new HashMap() ++ (localNames zip globalNames)

      def rename(id: Ident) = if (bindingsMap contains id) bindingsMap(id) else id
      val substitutedBody = substitute(rename, body)
      val (context3, resolvedBody, bodyStatements) = resolveExpr(context2, substitutedBody)

      val renamedBindings = (globalNames zip bindings) collect {
        case (name, a @ Assignment(_, params, body)) => {
          def renameWithParamsShadowed(id: Ident) = if (params contains id) id else rename(id)
          a copy (
            name = name,
            definition = substitute(renameWithParamsShadowed, body)
          )
        }
      }

      val (context4, globalBindings) = resolveStmts(context3, renamedBindings)

      (context4, resolvedBody, globalBindings ++ bodyStatements)
    }

    case _: Num => (context, expr, Seq())

    case b @ BinOp(l, _, r, _) => {
      val (context2, lResolved, lStatements) = resolveExpr(context, l)
      val (context3, rResolved, rStatements) = resolveExpr(context2, r)
      (context3, b.copy(left = lResolved, right = rResolved), lStatements ++ rStatements)
    }

    case c @ Chain(body, _) => {
      val (context2, bodyResolved, bodyStatements) = resolveExprs(context, body)
      (context2, c.copy(body = bodyResolved), bodyStatements)
    }

    case p @ Parallel(body, _) => {
      val (context2, bodyResolved, bodyStatements) = resolveExprs(context, body)
      (context2, p.copy(body = bodyResolved), bodyStatements)
    }

    case a @ Application(name, args, _) => {
      val (context2, argsResolved, argsStatements) = resolveExprs(context, args)
      val appResolved = a copy (args = argsResolved)
      (context2, appResolved, argsStatements)
    }

  }

  def resolveExprs(context: Context, exprs: Seq[Expr]): (Context, Seq[Expr], Seq[Statement]) =
    if (exprs.isEmpty) {
      (context, Seq(), Seq())
    } else {
      val (context2, headResolved, headStatements) = resolveExpr(context, exprs.head)
      val (context3, tailResolved, tailStatements) = resolveExprs(context2, exprs.tail)
      (context3, headResolved +: tailResolved, headStatements ++ tailStatements)
    }

  /**
   * Enable the given assignments to capture the given variables, which are local in expr. This
   * is achieved by inserting extra parameters in each assignment, and having the expression bind
   * the local variables to those parameters when applying one of the bindings.
   *
   * The expression should have already had all of its lets resolved, and the statements given
   * should be the global assignments generated by resolving the lets.
   */
  def capture(captures: Ident*)(stmts: Seq[Statement], expr: Expr): (Seq[Statement], Expr) =
    if (stmts.isEmpty) {
      // If no assignments were generated, there are no applications which require captures
      (stmts, expr)
    } else {
      val (stmtIds, stmtsWithParams): (Seq[Ident], Seq[Assignment]) = (stmts collect {
        case a @ Assignment(name, params, _) => {
          // If a captured variable has the same name as one of the existing parameters, then the
          // capture is shadowed by the parameter. Thus, we simply prefix it with an underscore, and
          // it will be ignored.
          val mangledCaptures = captures collect {
            case id @ Ident(name) if params contains id => id.copy(name = "_" + name)
            case id => id
          }
          (name, a.copy(params = mangledCaptures ++ params))
        }
      }).unzip

      require(stmtsWithParams.length == stmts.length)

      val idSet = new HashSet() ++ stmtIds

      val resolvedExpr = resolveCaptures(expr, idSet, captures)

      // We have to resolve the body of each assignment as well. Since recursive definitions are
      // prohibited, we can take out the statement we're resolving when doing so.
      val resolvedStmts = stmtsWithParams collect {
        case a @ Assignment(id, _, body) => a.copy(definition = resolveCaptures(body, idSet - id, captures))
      }

      (resolvedStmts, resolvedExpr)
    }

  // Helper functions

  def letNames(context: Context, basenames: Seq[Ident]): (Context, Seq[Ident]) = {
    val letNum = context.lets
    (context copy (lets = letNum + 1), basenames map (id => id.copy(name = s"_let$letNum${id.name}")))
  }

  def substitute(rename: Ident => Ident, expr: Expr): Expr = expr match {
    case n: Num => expr

    case b @ BinOp(l, _, r, _) => b copy (
      left = substitute(rename, l),
      right = substitute(rename, r)
    )

    case c @ Chain(body, _) => c copy (
      body = body map (substitute(rename, _))
    )

    case p @ Parallel(body, _) => p copy (
      body = body map (substitute(rename, _))
    )

    case a @ Application(id, args, _) => a copy (
      name = rename(id),
      args = args map (substitute(rename, _))
    )

    case l @ Let(bindings, body, _) => {

      val (renameWithBindingsShadowed, substitutedBindings) = substituteBindings(rename, bindings)

      l copy (
        bindings = substitutedBindings,
        body = substitute(renameWithBindingsShadowed, body)
      )

    }

  }

  def substituteBindings(rename: Ident => Ident, bindings: Seq[Assignment]):
    (Ident => Ident, Seq[Assignment]) =
    if (bindings.isEmpty) {
      (rename, Seq())
    } else {
      val a @ Assignment(name, params, body) = bindings.head

      def renameWithHeadShadowed(id: Ident) = if (id == name) id else rename(id)
      val (renameWithBindingsShadowed, substitutedTail) =
        substituteBindings(renameWithHeadShadowed, bindings.tail)

      def renameWithHeadAndParamsShadowed(id: Ident) =
        if (params contains id) id else renameWithHeadShadowed(id)
      val substitutedBody = substitute(renameWithHeadAndParamsShadowed, body)

      val substitutedHead = a copy (
        name = rename(name),
        definition = substitutedBody
      )

      (renameWithBindingsShadowed, substitutedHead +: substitutedTail)
    }

  def resolveChannels(context: Context, channels: Seq[Expr]): (Context, Seq[Expr], Seq[Statement]) =
    if (channels.isEmpty) {
      (context, Seq(), Seq())
    } else {
      val (context2, resolvedHead, headStatements) = resolveExpr(context, channels.head)
      val (context3, resolvedTail, tailStatements) = resolveChannels(context2, channels.tail)
      (context3, resolvedHead +: resolvedTail, headStatements ++ tailStatements)
    }

  def resolveCaptures(expr: Expr, applications: Set[Ident], captures: Seq[Ident]): Expr =
    if (applications.isEmpty) {
      expr
    } else {
      expr match {
        case n: Num => expr

        case b @ BinOp(l, _, r, _) => b copy (
          left = resolveCaptures(l, applications, captures),
          right = resolveCaptures(r, applications, captures)
        )

        case c @ Chain(body, _) => c copy (
          body = body map (resolveCaptures(_, applications, captures))
        )

        case p @ Parallel(body, _) => p copy (
          body = body map (resolveCaptures(_, applications, captures))
        )

        case a @ Application(id, args, _) => {
          val resolvedArgs = args map (resolveCaptures(_, applications, captures))

          if (applications contains id) {
            // All args have type number
            val captureArgs = captures map (Application(_, Seq()) annotated Number)
            a.copy(args = captureArgs ++ resolvedArgs)
          } else {
            a.copy(args = resolvedArgs)
          }
        }

        case l @ Let(bindings, body, _) => throw new SwTranslateError(l.loc, "Unresolved let.")
      }
    }

}

/**
 * The job of this object is to transform an abstract syntax tree by wrapping all components,
 * assignments, and instruments in a Node structure, which contains information about the inputs
 * and outputs of components, procedure calls, and instruments. The inputs and outputs are
 * audio-rate variables allocated by SwDag, which ensures that all variables are unique in their
 * scope.
 *
 * SwDag also ensures that if, for example, the output of component1 connects to the input of
 * component2, then component1's output variable is the same as component2's input variable. This
 * can be thought of as created a directed acyclic graph, whose nodes are components and whose edges
 * are labeled with audio-rate variable names. This graph reflects the underlying structure of audio
 * signal flows in the program.
 */
object SwDag {

  object Nodes {
    abstract trait Node extends ASTElem {
      val inputs: Seq[String]
      val outputs: Seq[String]
    }

    abstract class ExprNode extends Expr with Node
    abstract class StmtNode extends Statement with Node

    case class CompNode(inputs: Seq[String], outputs: Seq[String], comp: Expr) extends ExprNode {
      val ty: Option[SwType] = None
      def unannotatedString = comp.unannotatedString
      def annotated(newTy: SwType) = {
        throw new SwTranslateError(this.loc, "CompNode cannot be annotated.")
      }
    }

    case class AssignNode(inputs: Seq[String], outputs: Seq[String], assignment: Assignment)
      extends StmtNode

    case class InstrNode(inputs: Seq[String], outputs: Seq[String], instrument: Instrument)
      extends StmtNode
  }

  import Nodes._

  case class Context(signals: Int)

  object EmptyContext extends Context(0)

  def apply(stmts: Seq[Statement]): Either[SwCompileError, Seq[StmtNode]] =
    try {
      Right(stmts map transformStmt _)
    } catch {
      case e: SwCompileError => Left(e)
    }

  def typeOf[T <: TypeAnnotation with SwPositional](elem: T) = elem.ty match {
    case Some(Function(ty, _)) => ty
    case Some(ty) => ty
    case None =>
      throw new SwTranslateError(elem.loc, s"Untyped expression ${elem}.")
  }

  def getType[T, U <: TypeAnnotation with SwPositional](
    elem: U, expected: String, f: PartialFunction[SwType, T]) =
  {
    val ty = typeOf(elem)
    if (f.isDefinedAt(ty)) {
      f(ty)
    } else {
      throw new SwTranslateError(elem.loc, s"Unexpected type annotation $ty (expected $expected).")
    }
  }

  def signalAnons(context: Context, num: Int): (Context, Seq[String]) =
    if (num == 0) {
      (context, Seq())
    } else {
      val id = context.signals
      val (context2, names) = signalAnons(context.copy(signals = id + 1), num - 1)
      (context2, s"a$id" +: names)
    }

  def transformStmt(stmt: Statement): StmtNode = stmt match {

    case a @ Assignment(name, params, body) => {

      val (inputs, transformedBody, outputs) = typeOf(body) match {
        case Component(inArity, _) => {
          val (context, inputs) = signalAnons(EmptyContext, inArity)
          val (_, transformedComp, outputs) = transformExpr(context, body, inputs)
          (inputs, transformedComp, outputs)
        }
        case _ => {
          // Not a component, and thus has no inputs or outputs
          (Seq(), body, Seq())
        }
      }

      AssignNode(inputs, outputs, Assignment(name, params, transformedBody))
    }

    case i @ Instrument(channels, body, sends) => {
      // An instrument must be of component type
      val inArity = getType(body, "source", { case Component(in, _) => in })
      val (context, inputs) = signalAnons(EmptyContext, inArity)
      val (_, transformedBody, outputs) = transformExpr(context, body, inputs)

      val transformedSends = sends match {
        case None => None
        case Some(s) => {
          val inArity = getType(s, "effect", { case Component(in, _) => in })

          // The sends block will be translated as a separate instrument, so we can start with a
          // fresh context.
          val (context, inputs) = signalAnons(EmptyContext, inArity)
          val (_, transformedSends, _) = transformExpr(context, s, inputs)
          Some(transformedSends)
        }
      }

      InstrNode(inputs, outputs, Instrument(channels, transformedBody, transformedSends))
    }

  }

  def transformExpr(
    context: Context, expr: Expr, inputs: Seq[String]): (Context, CompNode, Seq[String]) =
    expr match {
      case Chain(body, ty) => {
        val (context2, transformedBody, outputs) = transformCompsSerial(context, body, inputs)
        (context2, CompNode(inputs, outputs, Chain(transformedBody, ty)), outputs)
      }

      case Parallel(body, ty) => {
        val (context2, transformedBody, outputs) = transformCompsParallel(context, body, inputs)
        (context2, CompNode(inputs, outputs, Parallel(transformedBody, ty)), outputs)
      }

      case _: Let => throw new SwTranslateError(expr.loc, "Unresolved let.")

      case app => {
        val outArity = getType(app, "component", { case Component(_, out) => out })
        val (context2, outputs) = signalAnons(context, outArity)
        (context2, CompNode(inputs, outputs, app), outputs)
      }
    }

  def transformCompsSerial(context: Context, comps: Seq[Expr], inputs: Seq[String]):
    (Context, Seq[CompNode], Seq[String]) =
    if (comps.isEmpty) {
      // An empty chain passes its inputs through unchanged
      (context, Seq(), inputs)
    } else {

      // Pipe inputs through to the first component
      val (context2, head, headOutputs) = transformExpr(context, comps.head, inputs)

      if (comps.length == 1) {
        (context2, Seq(head), headOutputs)
      } else {
        // Outputs of the first component become inputs to the next
        val (context3, tail, tailOutputs) = transformCompsSerial(context2, comps.tail, headOutputs)

        // Outputs of the overall chain are just the outputs of the last component
        (context3, head +: tail, tailOutputs)
      }
    }

  def transformCompsParallel(context: Context, comps: Seq[Expr], inputs: Seq[String]):
    (Context, Seq[CompNode], Seq[String]) =
    if (comps.isEmpty) {
      // An empty chain passes its inputs through unchanged
      (context, Seq(), inputs)
    } else {

      // Take as many inputs as we need from the pool of available inputs and pipe them through to
      // the first component.
      val inArity = getType(comps.head, "component", { case Component(in, _) => in })
      val headInputs = inputs.take(inArity)
      val (context2, head, headOutputs) = transformExpr(context, comps.head, headInputs)

      // Get the remaining inputs
      val tailInputs = inputs.drop(inArity)

      if (comps.length == 1) {
        require(tailInputs.isEmpty) // Typechecker should have ensured this
        (context2, Seq(head), headOutputs)
      } else {
        val (context3, tail, tailOutputs) = transformCompsParallel(context2, comps.tail, tailInputs)

        // Outputs of the overall parallel block are the outputs from each component
        (context3, head +: tail, headOutputs ++ tailOutputs)
      }
    }
}
