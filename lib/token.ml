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

let rec show_token_value token =
  match token.kind with
  | LET -> "let"
  | FUN -> "fun"
  | RETURN -> "return"
  | IF -> "if"
  | ELSE -> "else"
  | WHILE -> "while"
  | ID id -> id
  | INT_CONST i -> string_of_int i
  | STR_LIT s -> s
  | LPAREN -> "("
  | RPAREN -> ")"
  | COLON -> ":"
  | LBRACK -> "{"
  | RBRACK -> "}"
  | SEMICOLON -> ";"
  | COMMA -> ","
  | PLUS -> "+"
  | MINUS -> "-"
  | MULT -> "*"
  | DIV -> "/"
  | ASSIGN -> "="
  | EQUAL -> "=="
  | NEQUAL -> "!="
  | LT -> "<"
  | LE -> "<="
  | GT -> ">"
  | GE -> ">="
  | EOF -> "eof"

and get_token_kind token = token.kind

and get_kind_list tokens =
  match tokens with
  | [] -> []
  | token :: tokens -> token.kind :: get_kind_list tokens
