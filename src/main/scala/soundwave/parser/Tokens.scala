package soundwave.tokens

import scala.util.parsing.input.Positional

import soundwave._

abstract class SwToken(name: String) extends SwPositional {
    override def toString = name
}

case class IDENT(str: String) extends SwToken(s"identifier ($str)")
case class NUMBER(num: Double) extends SwToken(s"number literal (${num.toString})")
case class FILE(str: String) extends SwToken(s"file ($str)")

// Sadly, these must all be case classes (not objects) because they have a mutable position field
case class IMPORT() extends SwToken("keyword (import)")
case class INSTR() extends SwToken("keyword (instr)")
case class INSERTS() extends SwToken("keyword (inserts)")
case class SENDS() extends SwToken("keyword (sends)")
case class PARALLEL() extends SwToken("keyword (parallel)")
case class LET() extends SwToken("keyword (let)")
case class IN() extends SwToken("keyword (in)")
case class LPAREN() extends SwToken("(")
case class RPAREN() extends SwToken(")")
case class LBRACE() extends SwToken("{")
case class RBRACE() extends SwToken("}")
case class COMMA() extends SwToken(",")
case class EQUALS() extends SwToken("=")
case class STAR() extends SwToken("*")
case class SLASH() extends SwToken("/")
case class PLUS() extends SwToken("+")
case class MINUS() extends SwToken("-")
