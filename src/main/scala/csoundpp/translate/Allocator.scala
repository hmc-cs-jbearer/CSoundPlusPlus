package cspp

import scala.collection.immutable.HashMap

import absyn._

/**
 * This object allocates various Csound identifiers to aid in the translation. For example, it
 * allocates an a-rate variable for every audio signal in the program. See the objects below for
 * details on the operations applied.
 */
object CsppAllocator {
  def apply(stmts: Seq[Statement]): Either[CsppCompileError, Seq[CsppDag.Nodes.StmtNode]] =
    for {
      resolvedLets <- CsppLetResolver(stmts).right
      dag <- CsppDag(resolvedLets).right
    } yield dag
}

/**
 * The job of this object is to transform an abstract syntax tree by allocating unique global names
 * for every assignment made in a let statement, and to bring those assignments to the global scope
 * with the modified names, and to substitute the modified names into the body of the let expression
 * wherever the assignments are referenced.
 *
 * This is done because Csound will not let us declare "local" functions -- that would be opcodes
 * nested withing other opcodes or instruments. We can declare local variables (cspp constants) and
 * hopefully we will eventually optimie this by "inlining" the variables and only "globalizing" the
 * functions/components.
 *
 * For now, though, it is much simpler to globalize everything.
 */
object CsppLetResolver {

  case class Context(lets: Int)
  object Context {
    object Empty extends Context(0)
  }

  def apply(stmts: Seq[Statement]): Either[CsppCompileError, Seq[Statement]] = {
    val renamedStmts = stmts collect {
      case a @ Assignment(name, _, _) => a.copy(name = unletName(name))
      case i: Instrument => i
    }

    try {
      Right(resolveStmts(Context.Empty, renamedStmts)._2)
    } catch {
      case e: CsppCompileError => Left(e)
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
      val resolvedAsn = a copy (
        params = params map (unletName(_)),
        definition = resolvedBody
      )
      (context2, emittedStmts :+ resolvedAsn)
    }

    case i @ Instrument(channels, body, sends) => {
      val (context2, resolvedChannels, channelStmts) = resolveChannels(context, channels)
      val (context3, resolvedBody, bodyStmts) = resolveExpr(context2, body)
      val (context4, resolvedSends, sendsStmts) = sends match {
        case Some(s) => {
          val (context4, resolvedSends, sendsStmts) = resolveExpr(context3, s)
          (context4, Some(resolvedSends), sendsStmts)
        }
        case None    => (context3, None, Seq())
      }
      val resolvedInstr = i copy (
        channels = resolvedChannels,
        definition = resolvedBody,
        sends = resolvedSends
      )
      (context3, (channelStmts ++ bodyStmts ++ sendsStmts) :+ resolvedInstr)
    }
  }

  def resolveExpr(context: Context, expr: Expr, inLet: Boolean = false):
    (Context, Expr, Seq[Statement]) = expr match {

    case Let(bindings, body, ty) => {
      val localNames = bindings map { case Assignment(name, _, _) => name }
      val (context2, globalNames) = letNames(context, localNames)
      val bindingsMap = new HashMap() ++ (localNames zip globalNames)

      def rename(id: Ident) = if (bindingsMap contains id) bindingsMap(id) else unletName(id)
      val substitutedBody = substitute(rename, body)
      val (context3, resolvedBody, bodyStatements) =
        resolveExpr(context2, substitutedBody, inLet = true)

      val renamedBindings = (globalNames zip bindings) map {
        case (name, binding) => binding.copy(name = name)
      }

      val (context4, globalBindings) = resolveStmts(context3, renamedBindings)

      (context4, resolvedBody, globalBindings ++ bodyStatements)
    }

    case _: Num => (context, expr, Seq())

    case b @ BinOp(l, _, r, _) => {
      val (context2, lResolved, lStatements) = resolveExpr(context, l, inLet)
      val (context3, rResolved, rStatements) = resolveExpr(context2, r, inLet)
      (context3, b.copy(left = lResolved, right = rResolved), lStatements ++ rStatements)
    }

    case c @ Chain(body, _) => {
      val (context2, bodyResolved, bodyStatements) = resolveExprs(context, body, inLet)
      (context2, c.copy(body = bodyResolved), bodyStatements)
    }

    case p @ Parallel(body, _) => {
      val (context2, bodyResolved, bodyStatements) = resolveExprs(context, body, inLet)
      (context2, p.copy(body = bodyResolved), bodyStatements)
    }

    case a @ Application(name, args, _) => {
      val (context2, argsResolved, argsStatements) = resolveExprs(context, args, inLet)
      val appResolved = a copy (
        name = if (inLet) name else unletName(name),
        args = argsResolved
      )
      (context2, appResolved, argsStatements)
    }

  }

  def resolveExprs(context: Context, exprs: Seq[Expr], inLet: Boolean = true):
    (Context, Seq[Expr], Seq[Statement]) =
    if (exprs.isEmpty) {
      (context, Seq(), Seq())
    } else {
      val (context2, headResolved, headStatements) = resolveExpr(context, exprs.head, inLet)
      val (context3, tailResolved, tailStatements) = resolveExprs(context2, exprs.tail, inLet)
      (context3, headResolved +: tailResolved, headStatements ++ tailStatements)
    }

  // Helper functions

  def letNames(context: Context, basenames: Seq[Ident]): (Context, Seq[Ident]) = {
    val letNum = context.lets
    (context copy (lets = letNum + 1), basenames map (id => id.copy(name = s"let$letNum${id.name}")))
  }

  // Since we're prefixing let globals with letn, we have to prefix nonlet globals with something
  // else, so that the user cannot mess things up by naming a variable, eg, let0x
  def unletName(id: Ident) = id copy (name = s"_${id.name}")

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

}

/**
 * The job of this object is to transform an abstract syntax tree by wrapping all components,
 * assignments, and instruments in a Node structure, which contains information about the inputs
 * and outputs of components, procedure calls, and instruments. The inputs and outputs are
 * audio-rate variables allocated by CsppDag, which ensures that all variables are unique in their
 * scope.
 *
 * CsppDag also ensures that if, for example, the output of component1 connects to the input of
 * component2, then component1's output variable is the same as component2's input variable. This
 * can be thought of as created a directed acyclic graph, whose nodes are components and whose edges
 * are labeled with audio-rate variable names. This graph reflects the underlying structure of audio
 * signal flows in the program.
 */
object CsppDag {

  object Nodes {
    abstract trait Node extends ASTElem {
      val inputs: Seq[String]
      val outputs: Seq[String]
    }

    abstract class ExprNode extends Expr with Node
    abstract class StmtNode extends Statement with Node

    case class CompNode(inputs: Seq[String], outputs: Seq[String], comp: Expr) extends ExprNode {
      val ty: Option[CsppType] = None
      def annotated(newTy: CsppType) = {
        throw new CsppTranslateError(this.loc, "CompNode cannot be annotated.")
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

  def apply(stmts: Seq[Statement]): Either[CsppCompileError, Seq[StmtNode]] =
    try {
      Right(stmts map transformStmt _)
    } catch {
      case e: CsppCompileError => Left(e)
    }

  def typeOf[T <: TypeAnnotation with CsppPositional](elem: T) = elem.ty match {
    case Some(Function(ty, _)) => ty
    case Some(ty) => ty
    case None =>
      throw new CsppTranslateError(elem.loc, s"Untyped expression ${elem}.")
  }

  def getType[T, U <: TypeAnnotation with CsppPositional](
    elem: U, expected: String, f: PartialFunction[CsppType, T]) =
  {
    val ty = typeOf(elem)
    if (f.isDefinedAt(ty)) {
      f(ty)
    } else {
      throw new CsppTranslateError(elem.loc, s"Unexpected type annotation $ty (expected $expected).")
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

      case _: Let => throw new CsppTranslateError(expr.loc, "Unresolved let.")

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
