package soundwave

import scala.sys.process.{Process,ProcessLogger}

object SwPlayer {
  def apply(config: Config) = {
    val status = play(config.inFile, config.score, config.outFile)

    status.left.flatMap {
      case CSoundError(code) => { System.exit(code); Left(()) }
      case err               => { System.err.println(err); System.exit(1); Left(()) }
    }
  }

  def play(sourceFile: String, scoreFile: String, outFileOpt: String): Either[CSoundError, Unit] = {
    val outFile = if (outFileOpt.isEmpty) "dac" else outFileOpt

    val csound = Process(Seq("python3", "bin/csound-play", sourceFile, scoreFile, "-o", outFile))
    val csoundIO = ProcessLogger(println(_), System.err.println(_))

    val exitCode = csound ! csoundIO

    if (exitCode == 0) {
      Right(())
    } else {
      Left(CSoundError(exitCode))
    }
  }
}
