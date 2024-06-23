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
[@@deriving show]

(* Token location *)
type location = { input_name : string; line : int; col : int }
[@@deriving show]

(* Token *)
type token = { kind : token_kind; loc : location } [@@deriving show]

(* Error *)
type error = { loc : location; msg : string }

(* Lexer *)
type t = {
  input : string;
  input_len : int;
  input_name : string;
  cur_char : char option;
  cursor : int;
  line : int;
  col : int;
}

val make : string -> string -> t
val show_error : error -> string
val get_token_kind : token -> token_kind
val lex : t -> (token * t, error) result
