package cspp

object Cspp extends App {

  def usage() = {
    System.err.println("Usage: Cspp <cspp file>")
    System.exit(1)
  }

  def compile(source: String): Either[CsppCompileError, CsppTranslator.CsLines] = {
    for {
      tokens <- CsppLexer(source).right
      ast <- CsppParser(tokens).right
      annotated <- CsppTypeChecker(ast).right
      csound <- CsppTranslator(annotated).right
    } yield csound
  }

  if (args.length != 1) {
    usage()
  }

  val source = io.Source.fromFile(args(0)).getLines.mkString("\n")
  compile(source) match {
    case Right(output) => println(output.mkString("\n"))
    case Left(err) => {
      System.err.println(err)
      System.exit(1)
    }
  }

}
