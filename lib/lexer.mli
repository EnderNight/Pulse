open Error

type token_kind =
  (* Keywords *)
  | LET
  | FUN
  | RETURN
  | IF
  | ELSE
  | WHILE
  (* Identifier *)
  | ID of string
  (* Constants *)
  | INT_CONST of int
  (* String literal *)
  | STR_LIT of string
  (* Ponctuator *)
  | LPAREN
  | RPAREN
  | COLON
  | LBRACK
  | RBRACK
  | SEMICOLON
  | COMMA
  (* Operators *)
  | PLUS
  | MINUS
  | MULT
  | DIV
  | ASSIGN (* = *)
  | EQUAL (* == *)
  | NEQUAL (* != *)
  | LT (* < *)
  | LE (* <= *)
  | GT (* > *)
  | GE (* >= *)
  (* End of file *)
  | EOF

(* Token *)
type token = { kind : token_kind; loc : location }

(* Lexer *)
type lexer = {
  input : string;
  input_len : int;
  input_name : string;
  cur_char : char option;
  cursor : int;
  line : int;
  col : int;
}

val make : string -> string -> lexer
val make_loc : lexer -> location
val get_token_kind : token -> token_kind
val get_kind_list : token list -> token_kind list
val show_token_value : token -> string
val lex : lexer -> (token * lexer, error) result
