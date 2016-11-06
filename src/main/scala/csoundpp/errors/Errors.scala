package cspp

// Class used for reporting position of errors within the source file
case class Location(line: Int, column: Int) {
  override def toString = s"$line:$column"
}

trait CsppCompileError

case class CsppLexerError(location: Location, msg: String)
case class CsppParserError(location: Location, msg: String)
