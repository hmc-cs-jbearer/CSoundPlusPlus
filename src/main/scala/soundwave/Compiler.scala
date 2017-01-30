package soundwave

import tokens.{IMPORT,FILE}
import csound._

object SwCompiler {
  def apply(config: Config) {
    val importStdLib = Seq(IMPORT(), FILE("lib/standard.swav"))

    // We have to disable importing of the input file, to avoid cyclic imports
    val disabled = SwParser.disablingContext(config.inFile)

    // Compile the source file to get the human readable output, with whitespace and comments
    val verbose: Either[SwCompileError, SwTranslator.CsLines] = for {
      preamble <- SwFileReader("csound/preamble.csd").right
      source <- SwFileReader(config.inFile).right
      tokens <- SwLexer(source).right
      ast <- SwParser(importStdLib ++ tokens, disabled).right
      annotated <- SwTypeChecker(ast).right
      optimized <- SwOptimizer(annotated).right
      allocated <- SwAllocator(optimized).right
      csound <- SwTranslator(allocated, config.debug).right
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
        SwFileWriter(config.outFile, lines, exec = true)
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
