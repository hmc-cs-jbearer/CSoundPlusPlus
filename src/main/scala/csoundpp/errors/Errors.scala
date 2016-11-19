package cspp

import scala.language.implicitConversions
import scala.util.parsing.input.{Position,Positional,NoPosition}

// Class used for reporting position of errors within the source file
case class Location(line: Int, column: Int, file: String) extends Position {
  override def toString = s"$file:$line:$column"
  override def lineContents = ""
}

object NoLocation extends Location(0, 0, "")

trait CsppPositional extends Positional {
  var file: String = ""

  override def setPos(newPos: Position): CsppPositional.this.type = {
    if (pos == NoPosition) {
      pos = newPos
    }
    this
  }

  def inFile(newFile: String): CsppPositional.this.type = {
    file = newFile
    this
  }

  def loc: Location = Location(pos.line, pos.column, file)
}

class CsppCompileError(val location: Location, val msg: String) extends Throwable {
    override def toString = s"($location) $msg"
}

object CsppCompileError {
  def unapply(err: CsppCompileError): Option[(Location, String)] = Some((err.location, err.msg))
}

class CsppFileError(msg: String) extends CsppCompileError(NoLocation, msg)
class CsppLexerError(location: Location, msg: String) extends CsppCompileError(location, msg)
class CsppParserError(location: Location, msg: String) extends CsppCompileError(location, msg)
class CsppTypeError(location: Location, msg: String) extends CsppCompileError(location, msg)
class CsppTranslateError(location: Location, msg: String) extends CsppCompileError(location, msg)
