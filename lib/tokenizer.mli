type token =
  | LET
  | COLON
  | SEMICOLON
  | LPAREN
  | RPAREN
  | LBRACK
  | RBRACK
  | COMA
  | EQUAL
  | IF
  | ELSE
  | RETURN
  | WHILE
  (* Operators *)
  | PLUS
  | MINUS
  | MULT
  | DIV
  | LT
  | GT
  | LE
  | GE
  | EOF
  | IDENTIFIER of string
  | INT_LITERAL of int
  | STR_LITERAL of string

val lex : string -> token list
