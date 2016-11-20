package cspp

import scopt.OptionParser

import tokens.{IMPORT,FILE}
import csound._

object Cspp extends App {

  val config = ArgParser(args)

  val importStdLib = Seq(IMPORT(), FILE("lib/standard.csp"))

  // We have to disable importing of the input file, to avoid cyclic imports
  val disabled = CsppParser.disablingContext(config.inFile)

  // Compile the source file to get the human readable output, with whitespace and comments
  val verbose: Either[CsppCompileError, CsppTranslator.CsLines] = for {
    preamble <- CsppFileReader("csound/preamble.csd").right
    source <- CsppFileReader(config.inFile).right
    tokens <- CsppLexer(source).right
    ast <- CsppParser(importStdLib ++ tokens, disabled).right
    annotated <- CsppTypeChecker(ast).right
    csound <- CsppTranslator(annotated).right
  } yield preamble.contents.split("\n") ++ csound

  // If necessary, minify the human-readable output by removing whitespace and comments
  val output = if (config.humanReadable) verbose else {
    verbose.right.flatMap { lines =>
      CsoundMinifier(lines) match {
        case Right(minified) => Right(minified)
        case Left(err) => {
          // If we fail to minify it most likely means the CSound parser is broken. We'll fall back
          // to the verbose output and emit a warning.
          System.err.println(s"Warning: failed to minify generated CSound code:\n$err")
          Right(lines)
        }
      }
    }
  }

  // Write the result to the output file
  val result = output.right.flatMap { lines =>
    if (config.outFile.isEmpty) {
      Right(println(lines.mkString("\n")))
    } else {
      CsppFileWriter(config.outFile, lines)
    }
  }

  // If an error ocurred at any point during the process, report it now
  result.left.flatMap { err =>
    System.err.println(err)
    System.exit(1)
    Left(())
  }

}

object ArgParser {

  case class Config(
      inFile: String = "",
      outFile: String = "",
      humanReadable: Boolean = false
    )

  def apply(argv: Array[String]): Config = {

    new OptionParser[Config]("cspp") {

      head("cspp", "0.0.1")

      opt[Unit]('h', "human-readable").
      text("generate readble CSound code (results in larger output files)").
      action { (_, c) =>
        c.copy(humanReadable = true)
      }

      opt[String]('o', "out").
      text("place the output into <file> (default is stdout)").
      valueName("<file>").
      action { (x, c) =>
        c.copy(outFile = x)
      }

      arg[String]("file").required().
      text("the CSound++ source file to compile").
      action { (x, c) =>
        c.copy(inFile = x)
      }

      help("help").text("display this help and exit")

    }.parse(argv, Config()) match {
      case Some(config) => config
      case None         => { System.exit(1); Config() }
    }
  }
}
