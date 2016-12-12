package cspp

import absyn._
import CsppDagNodes._

object CsppDagNodes {
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

  case class SendsNode(inputs: Seq[String], outputs: Seq[String], sends: Sends) extends StmtNode
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

    case i @ Instrument(channels, body) => {
      // An instrument must be of component type
      val inArity = getType(body, "source", { case Component(in, _) => in })
      val (context, inputs) = signalAnons(EmptyContext, inArity)
      val (_, transformedBody, outputs) = transformExpr(context, body, inputs)
      InstrNode(inputs, outputs, Instrument(channels, transformedBody))
    }

    case s @ Sends(channel, body) => {
      val inArity = getType(body, "effect", { case Component(in, _) => in })
      val (context, inputs) = signalAnons(EmptyContext, inArity)
      val (_, transformedBody, outputs) = transformExpr(context, body, inputs)
      SendsNode(inputs, outputs, Sends(channel, transformedBody))
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
