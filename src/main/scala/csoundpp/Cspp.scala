package cspp

import scopt.OptionParser

object Cspp extends App {
  val config = ArgParser(args)
  config.command match {
    case Compile => CsppCompiler(config)
    case Play => CsppPlayer(config)
  }
}

abstract class Command
case object Compile extends Command
case object Play extends Command

case class Config(
  command: Command = Compile,

  inFile: String = "",
  outFile: String = "",
  humanReadable: Boolean = false,
  score: String = "",
  debug: Boolean = false
)

object ArgParser {

  def apply(argv: Array[String]): Config = {

    new OptionParser[Config]("cspp") {

      head("cspp", "0.0.1")

      cmd("compile").
        text("compile a CSound++ program").
        maxOccurs(1).
        action( (_, c) => c.copy(command = Compile) ).
        children(

          opt[Unit]('h', "human-readable").
            text("generate readble CSound code (results in larger output files)").
            action { (_, c) =>
              c.copy(humanReadable = true)
            },

          opt[Unit]('d', "debug").
            text("compile with debug information, which can be used by CSPP analysis tools").
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
            text("the CSound++ source file to compile").
            action { (x, c) =>
              c.copy(inFile = x)
            }
        )

        cmd("play").
          text("play a MIDI score with a compiled CSound++ program").
          maxOccurs(1).
          action( (_, c) => c.copy(command = Play) ).
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

    }.parse(argv, Config()) match {
      case Some(config) => config
      case None         => { System.exit(1); Config() }
    }
  }
}
