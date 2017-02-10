package soundwave

import scala.language.implicitConversions
import scala.util.parsing.input.{Position,Positional,NoPosition}
import java.io._

// Class used for reporting position of errors within the source file
case class Location(line: Int, column: Int, file: String) extends Position {
  override def toString = s"$file:$line:$column"
  override def lineContents = ""
}

object NoLocation extends Location(0, 0, "")

trait SwPositional extends Positional {
  var file: String = ""

  override def setPos(newPos: Position): SwPositional.this.type = {
    if (pos == NoPosition) {
      pos = newPos
    }
    this
  }

  def inFile(newFile: String): SwPositional.this.type = {
    file = newFile
    this
  }

  def loc: Location = Location(pos.line, pos.column, file)
}

abstract class SwError extends Throwable {
  override def getMessage = toString
}

case class SwInternalError(msg: String) extends SwError {
  lazy val trace = {
    val sw = new StringWriter()
    printStackTrace(new PrintWriter(sw))
    sw.toString
  }

  override def toString =
    s"SoundWave has encountered a fatal error.\nDetails: $msg"

  override def getMessage = trace
}

class SwCompileError(val location: Location, val msg: String) extends SwError {
    override def toString = s"($location) $msg"
}

object SwCompileError {
  def unapply(err: SwCompileError): Option[(Location, String)] = Some((err.location, err.msg))
}

class SwFileError(msg: String) extends SwCompileError(NoLocation, msg)
class SwLexerError(location: Location, msg: String) extends SwCompileError(location, msg)
class SwParserError(location: Location, msg: String) extends SwCompileError(location, msg)
class SwTypeError(location: Location, msg: String) extends SwCompileError(location, msg)
class SwTranslateError(location: Location, msg: String) extends SwCompileError(location, msg)

class CsParseError(val position: Position, val msg: String) {
  override def toString = s"($position) $msg"
}

case class CSoundError(code: Int)
