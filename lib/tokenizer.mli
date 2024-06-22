type token =
  (* Constants *)
  | IDENTIFIER of string
  | INT_LITERAL of int
  | STR_LITERAL of string
  (* Reserved words *)
  | LET
  | IF
  | ELSE
  | RETURN
  | WHILE
  (* Structure *)
  | COLON
  | SEMICOLON
  | LPAREN
  | RPAREN
  | LBRACK
  | RBRACK
  | COMA
  (* Operators *)
  | PLUS
  | MINUS
  | MULT
  | DIV
  | LT
  | GT
  | LE
  | GE
  | ASSIGN (* = *)
  | EQUAL (* == *)
  | DIFF (* != *)
  (* Misc *)
  | EOF
[@@deriving show]

val string_of_token : token -> string
val lex : string -> token list
