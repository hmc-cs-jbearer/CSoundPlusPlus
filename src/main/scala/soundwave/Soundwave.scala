package soundwave

import scopt.OptionParser

object Soundwave extends App {
  val config = ArgParser(args)
  config.command match {
    case Some(Compile) => SwCompiler(config)
    case Some(Play) => SwPlayer(config)
    case _ => ArgParser.showUsage()
  }
}

abstract class Command
case object Compile extends Command
case object Play extends Command

case class Config(
  command: Option[Command] = None,

  inFile: String = "",
  outFile: String = "",
  humanReadable: Boolean = false,
  score: String = "",
  debug: Boolean = false
)

object ArgParser {

  def apply(argv: Array[String]): Config =
    parser.parse(argv, Config()) match {
      case Some(config) => config
      case None         => { System.exit(1); Config() }
    }

  def showUsage() = parser.showUsage

  val parser = new OptionParser[Config]("soundwave") {

    head(BuildInfo.name, BuildInfo.version)

    cmd("compile").
      text("compile a SoundWave program").
      maxOccurs(1).
      action( (_, c) => c.copy(command = Some(Compile)) ).
      children(

        opt[Unit]('h', "human-readable").
          text("generate readble CSound code (results in larger output files)").
          action { (_, c) =>
            c.copy(humanReadable = true)
          },

        opt[Unit]('d', "debug").
          text("compile with debug information, which can be used by SoundWave analysis tools").
          action { (_, c) =>
            c.copy(debug = true)
          },

        opt[String]('o', "out").
          text("place the output into <file> (default is stdout)").
          valueName("<file>").
          action { (x, c) =>
            c.copy(outFile = x)
          },

        arg[String]("file").required().
          text("the SoundWave source file to compile").
          action { (x, c) =>
            c.copy(inFile = x)
          }
      )

      cmd("play").
        text("play a MIDI score with a compiled SoundWave program").
        maxOccurs(1).
        action( (_, c) => c.copy(command = Some(Play)) ).
        children(

          opt[String]('o', "out").
            valueName("<file>").
            text("place the output into <file> in WAV format").
            action { (x, c) =>
              c.copy(outFile = x)
            },

          arg[String]("program").required().
            text("the file containing the program to run").
            action { (x, c) =>
              c.copy(inFile = x)
            },

          arg[String]("score").required().
            text("the MIDI file containing the score to perform").
            action { (x, c) =>
              c.copy(score = x)
            }

        )

    help("help").text("display this help and exit")

  }
}
