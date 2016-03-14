header {
package edu.mit.compilers.grammar;
}

options
{
  mangleLiteralPrefix = "TK_";
  language = "Java";
}

class DecafParser extends Parser;
options
{
  importVocab = DecafScanner;
  k = 3;
  buildAST = true;
}

tokens
{
  ARRAY_FIELD;
  BLOCK;
  CALLOUT_DECLS;
  FIELD_DECL;
  FIELD_DECLS;
  METHOD_ARGS;
  METHOD_CALL;
  METHOD_DECL;
  METHOD_DECLS;
  METHOD_PARAM;
  METHOD_PARAMS;
  PROGRAM;
  TYPE;
  VARIABLE_FIELD;
}

// Java glue code that makes error reporting easier.
// You can insert arbitrary Java code into your parser/lexer this way.
{
  // Do our own reporting of errors so the parser can return a non-zero status
  // if any errors are detected.
  /** Reports if any errors were reported during parse. */
  private boolean error;
  private String errorMsg;

  @Override
  public void reportError (RecognitionException ex) {
    // Print the error via some kind of error reporting mechanism.
    error = true;
    errorMsg = ex.toString();
  }
  @Override
  public void reportError (String s) {
    // Print the error via some kind of error reporting mechanism.
    error = true;
  }
  public boolean getError () {
    return error;
  }
  public String getErrorMsg () {
    return errorMsg;
  }

  // Selectively turns on debug mode.

  /** Whether to display debug information. */
  private boolean trace = false;

  public void setTrace(boolean shouldTrace) {
    trace = shouldTrace;
  }
  @Override
  public void traceIn(String rname) throws TokenStreamException {
    if (trace) {
      super.traceIn(rname);
    }
  }
  @Override
  public void traceOut(String rname) throws TokenStreamException {
    if (trace) {
      super.traceOut(rname);
    }
  }
}

program : calloutdecls fielddecls methoddecls EOF!
        { #program = #([PROGRAM,"program"], #program); };

calloutdecls : (calloutdecl)*
             { #calloutdecls = #([CALLOUT_DECLS,"calloutdecls"], #calloutdecls); };
calloutdecl : TK_callout^ IDENTIFIER SEMICOLON!;

fielddecls : (fielddecl)*
             { #fielddecls = #([FIELD_DECLS,"fielddecls"], #fielddecls); };
fielddecl : type field (COMMA! field)* SEMICOLON!
             { #fielddecl = #([FIELD_DECL,"fielddecl"], #fielddecl); };
field : variable_field | array_field;
variable_field : IDENTIFIER
             { #variable_field = #([VARIABLE_FIELD,"variable_field"], #variable_field); };
array_field : IDENTIFIER LBRACKET! INTLITERAL RBRACKET!
             { #array_field = #([ARRAY_FIELD,"array_field"], #array_field); };

methoddecls : (methoddecl)*
             { #methoddecls = #([METHOD_DECLS,"methoddecls"], #methoddecls); };
return_type : type | TK_void;
methoddecl : return_type methodname LPAREN! methodparams RPAREN! block
             { #methoddecl = #([METHOD_DECL,"methoddecl"], #methoddecl); };
methodparams : (methodparam (COMMA! methodparam)*)?
             { #methodparams = #([METHOD_PARAMS,"methodparams"], #methodparams); };
methodparam : type IDENTIFIER
             { #methodparam = #([METHOD_PARAM,"methodparam"], #methodparam); };

block : LBRACE! (fielddecl)* (statement)* RBRACE!
      { #block = #([BLOCK,"block"], #block); };

type : TK_int | TK_boolean;

statement :
      assignment
    | methodcall SEMICOLON!
    | if_
    | for_
    | while_
    | return_
    | break_
    | continue_
;

assignment : location (EQUALS^ | PLUSEQUALS^ | MINUSEQUALS^) expr SEMICOLON!;
if_ : TK_if^ LPAREN! if_condition RPAREN! block (else_)?;
if_condition: expr;
else_ : TK_else^ block;
for_ : TK_for^ LPAREN! for_init COMMA! for_end RPAREN! block;
for_init : IDENTIFIER EQUALS! expr;
for_end : expr;
while_ : TK_while^ LPAREN! while_condition RPAREN! (COLON! while_cap)? block;
while_condition : expr;
while_cap : INTLITERAL;
return_ : TK_return^ (expr)? SEMICOLON!;
break_ : TK_break^ SEMICOLON!;
continue_ : TK_continue^ SEMICOLON!;

methodcall : methodname LPAREN! (methodargs)? RPAREN!
      { #methodcall = #([METHOD_CALL,"methodcall"], #methodcall); };
methodargs : methodarg (COMMA! methodarg)*
             { #methodargs = #([METHOD_ARGS,"methodargs"], #methodargs); };
methodname : IDENTIFIER^;
methodarg : expr | STRINGLITERAL;

location : IDENTIFIER^ (LBRACKET! expr RBRACKET!)?;

expr :
    ternexp
;

atom : location | literal | LPAREN! ternexp RPAREN! | methodcall;
lengthexp : AT^ IDENTIFIER | atom;
minusexp : MINUS^ minusexp | lengthexp;
notexp : EXCL^ notexp | minusexp;
multexp : notexp ((TIMES^ | DIV^ | MOD^) notexp)*;
addexp : multexp ((PLUS^ | MINUS^) multexp)*;
compexp : addexp ((GT^ | LT^ | GTE^ | LTE^) addexp)*;
eqexp : compexp ((EQ^ | NEQ^) compexp)*;
andexp : eqexp (AND^ eqexp)*;
orexp : andexp (OR^ andexp)*;
ternexp : orexp (QUES^ ternexp COLON! ternexp)?;

literal : INTLITERAL | CHARLITERAL | TK_true | TK_false;
