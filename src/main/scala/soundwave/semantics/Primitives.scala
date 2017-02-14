package soundwave

import scala.collection.mutable.{Map,HashMap}

import absyn._
import AbsynSugar._

trait PrimitiveDefinitions {
  type PrimitiveDefinition = Function[Seq[Double], Expr]

  val types: Map[(Ident, Int), SwType] = new HashMap[(Ident, Int), SwType]()
  val definitions: Map[Ident, PrimitiveDefinition] = new HashMap[Ident, PrimitiveDefinition]()

  private var current: String = ""
  def unimplemented: Expr = throw SwUnimplementedError(current)

  def primitive(name: String, arity: Int, resultTy: SwType): (PrimitiveDefinition => Unit) = {
    current = s"$name($arity) [$resultTy]"
    return { (definition: PrimitiveDefinition) =>
      types += ((Ident(name), arity) -> resultTy)
      definitions += (Ident(name) -> { args =>
        if (args.length == arity) {
          definition(args)
        } else {
          throw SwRuntimeError(
            s"Wrong number of arguments to function $name: expected $arity, found ${args.length}.")
        }
      })
    }
  }

  def call(id: Ident, args: Seq[Num]): Expr =
    if (definitions contains id)
      definitions(id)(args map (_.value))
    else
      throw SwRuntimeError(s"Unknown primitive $id.")
}

object SwPrimitives extends PrimitiveDefinitions {
  primitive("foscil", 4, Component(0, 1)) { args =>
    unimplemented
  }

  primitive("sine", 2, Source) { args =>
    unimplemented
  }

  primitive("pulse", 2, Source) { args =>
    unimplemented
  }

  primitive("sidechain_compress", 5, Component(2, 1)) { args =>
    unimplemented
  }

  primitive("compress", 5, Effect) { args =>
    unimplemented
  }

  primitive("adsr", 4, Effect) { args =>
    unimplemented
  }

  primitive("average", 0, Component(4, 1)) { args =>
    unimplemented
  }

  primitive("delay", 2, Component(1, 1)) { args =>
    unimplemented
  }

  primitive("reverb", 1, Component(1, 1)) { args =>
    unimplemented
  }

  primitive("scale", 1, Component(1, 1)) { args =>
    unimplemented
  }

  primitive("transpose", 1, Component(1, 1)) { args =>
    unimplemented
  }

  primitive("filt", 4, Component(1, 1)) { args =>
    unimplemented
  }

  primitive("sqrt", 1, Number) { args =>
    Math.sqrt(args.head)
  }

  primitive("lopass_sweep", 6, Component(1, 1)) { args =>
    unimplemented
  }

  primitive("sum", 0, Component(4, 1)) { args =>
    unimplemented
  }

}
