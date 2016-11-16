package cspp

object Cspp extends App {

  def usage() = {
    System.err.println("Usage: Cspp <cspp file>")
    System.exit(1)
  }

  def readFile(path: String) = io.Source.fromFile(path).getLines.mkString("\n")

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

  val preamble = readFile("resources/csound/preamble.csd")
  val source = readFile(args(0))

  compile(source) match {
    case Right(output) => println(preamble ++ output.mkString("\n"))
    case Left(err) => {
      System.err.println(err)
      System.exit(1)
    }
  }

}
