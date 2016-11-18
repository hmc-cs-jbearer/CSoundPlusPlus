package cspp

object Cspp extends App {

  def usage() = {
    System.err.println("Usage: Cspp <cspp file>")
    System.exit(1)
  }

  def compile(path: String): Either[CsppCompileError, String] = {
    val importStdLib = Seq(IMPORT(), FILE("lib/standard.csp"))

    for {
      preamble <- CsppFileReader("csound/preamble.csd").right
      source <- CsppFileReader(path).right
      tokens <- CsppLexer(source).right
      ast <- CsppParser(importStdLib ++ tokens).right
      annotated <- CsppTypeChecker(ast).right
      csound <- CsppTranslator(annotated).right
    } yield preamble + csound.mkString("\n")
  }

  if (args.length != 1) {
    usage()
  }

  compile(args(0)) match {
    case Right(output) => println(output)
    case Left(err) => {
      System.err.println(err)
      System.exit(1)
    }
  }

}
