package cspp.tokens

import scala.util.parsing.input.Positional

import cspp._

abstract class CsppToken(name: String) extends CsppPositional {
    override def toString = name
}

case class IDENT(str: String) extends CsppToken(s"identifier ($str)")
case class NUMBER(num: Double) extends CsppToken(s"number literal (${num.toString})")
case class FILE(str: String) extends CsppToken(s"file ($str)")

// Sadly, these must all be case classes (not objects) because they have a mutable position field
case class IMPORT() extends CsppToken("keyword (import)")
case class INSTR() extends CsppToken("keyword (instr)")
case class INSERTS() extends CsppToken("keyword (inserts)")
case class SENDS() extends CsppToken("keyword (sends)")
case class PARALLEL() extends CsppToken("keyword (parallel)")
case class LPAREN() extends CsppToken("(")
case class RPAREN() extends CsppToken(")")
case class LBRACE() extends CsppToken("{")
case class RBRACE() extends CsppToken("}")
case class COMMA() extends CsppToken(",")
case class EQUALS() extends CsppToken("=")
case class STAR() extends CsppToken("*")
case class SLASH() extends CsppToken("/")
case class PLUS() extends CsppToken("+")
case class MINUS() extends CsppToken("-")
