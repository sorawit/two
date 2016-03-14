header {
package edu.mit.compilers.grammar;
}

options
{
  mangleLiteralPrefix = "TK_";
  language = "Java";
}

{@SuppressWarnings("unchecked")}
class DecafScanner extends Lexer;
options
{
  k = 2;
  testLiterals = false;
}

tokens
{
    "boolean";
    "break";
    "callout";
    "continue";
    "else";
    "false";
    "for";
    "while";
    "if";
    "int";
    "return";
    "true";
    "void";
}

// Selectively turns on debug tracing mode.
// You can insert arbitrary Java code into your parser/lexer this way.
{
  /** Whether to display debug information. */
  private boolean trace = false;

  public void setTrace(boolean shouldTrace) {
    trace = shouldTrace;
  }
  @Override
  public void traceIn(String rname) throws CharStreamException {
    if (trace) {
      super.traceIn(rname);
    }
  }
  @Override
  public void traceOut(String rname) throws CharStreamException {
    if (trace) {
      super.traceOut(rname);
    }
  }
}


LPAREN : '(';
RPAREN : ')';
LBRACKET : '[';
RBRACKET : ']';
LBRACE : '{';
RBRACE : '}';

COMMA : ',';
COLON : ':';
SEMICOLON : ';';
EQUALS : '=';
AT : '@';
PLUS : '+';
MINUS : '-';
EXCL : '!';
QUES : '?';
PLUSEQUALS : "+=";
MINUSEQUALS : "-=";

protected LITERAL : INTLITERAL | CHARLITERAL | IDENTIFIER;

TIMES : '*';
DIV : '/';
MOD : '%';

GT : '>';
LT : '<';
GTE : ">=";
LTE : "<=";

EQ : "==";
NEQ : "!=";

AND : "&&";
OR : "||";

IDENTIFIER options {testLiterals=true;}: ALPHA (ALPHANUM)*;

protected ALPHANUM : ALPHA | DECIMALDIGIT;

protected ALPHA : 'a'..'z' | 'A'..'Z' | '_';

INTLITERAL : DECIMALLITERAL | HEXLITERAL;

protected DECIMALLITERAL : DECIMALDIGIT (DECIMALDIGIT)*;

protected DECIMALDIGIT : '0'..'9';

protected HEXLITERAL : "0x" HEXDIGIT (HEXDIGIT)*;

protected HEXDIGIT : '0'..'9' | 'A'..'F' | 'a'..'f';

// Note that here, the {} syntax allows you to literally command the lexer
// to skip mark this token as skipped, or to advance to the next line
// by directly adding Java commands.
WS_ : (' ' | '\t' | '\n' {newline();}) {_ttype = Token.SKIP; };
SL_COMMENT : "//" (~'\n')* '\n' {_ttype = Token.SKIP; newline (); };

CHARLITERAL : '\'' ALLOWEDCHAR '\'';
STRINGLITERAL : '"' (ALLOWEDSTRING)* '"';

protected ALLOWEDCHAR : ESC|~('"'|'\n'|'\t'|'\''|'\\');

protected ALLOWEDSTRING : ESC|~('"'|'\n'|'\t'|'\''|'\\');

protected ESC :  '\\' ('"'|'\''|'\\'|'t'|'n');
