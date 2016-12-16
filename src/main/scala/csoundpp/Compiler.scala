package cspp

import tokens.{IMPORT,FILE}
import csound._

object CsppCompiler {
  def apply(config: Config) {
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
      optimized <- CsppOptimizer(annotated).right
      dag <- CsppDag(optimized).right
      csound <- CsppTranslator(dag, config.debug).right
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
        CsppFileWriter(config.outFile, lines, exec = true)
      }
    }

    // If an error ocurred at any point during the process, report it now
    result.left.flatMap { err =>
      System.err.println(err)
      System.exit(1)
      Left(())
    }
  }
}
