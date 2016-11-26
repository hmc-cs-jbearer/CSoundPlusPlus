package cspp.tokens

import scala.util.parsing.input.Positional

import cspp._

trait CsppToken extends CsppPositional

case class IDENT(str: String) extends CsppToken
case class NUMBER(num: Double) extends CsppToken
case class FILE(str: String) extends CsppToken

// Sadly, these must all be case classes (not objects) because they have a mutable position field
case class IMPORT() extends CsppToken
case class INSTR() extends CsppToken
case class PARALLEL() extends CsppToken
case class LPAREN() extends CsppToken
case class RPAREN() extends CsppToken
case class LBRACE() extends CsppToken
case class RBRACE() extends CsppToken
case class COMMA() extends CsppToken
case class EQUALS() extends CsppToken
case class STAR() extends CsppToken
case class SLASH() extends CsppToken
case class PLUS() extends CsppToken
case class MINUS() extends CsppToken
