package cspp

import scala.language.implicitConversions
import scala.util.parsing.input.Position

// Class used for reporting position of errors within the source file
case class Location(line: Int, column: Int) {
  override def toString = s"$line:$column"
}

object Location {
  implicit def Position2Location(pos: Position): Location = Location(pos.line, pos.column)
}

class CsppCompileError(val location: Location, val msg: String) extends Throwable {
    override def toString = s"$location: $msg"
}

object CsppCompileError {
  def unapply(err: CsppCompileError): Option[(Location, String)] = Some((err.location, err.msg))
}

class CsppLexerError(location: Location, msg: String) extends CsppCompileError(location, msg)
class CsppParserError(location: Location, msg: String) extends CsppCompileError(location, msg)
class CsppTypeError(location: Location, msg: String) extends CsppCompileError(location, msg)
class CsppTranslateError(location: Location, msg: String) extends CsppCompileError(location, msg)
