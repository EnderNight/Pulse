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
type t = { kind : token_kind; loc : Location.t }

val show_token_value : t -> string
val get_token_kind : t -> token_kind
val get_kind_list : t list -> token_kind list
