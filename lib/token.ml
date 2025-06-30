type token_kind =
  (* Constants *)
  | INT of string
  | IDENT of string
  (* Operators *)
  | PLUS
  | MINUS
  | MULT
  | DIV
  | EQ
  (* Punctuators *)
  | LPAREN
  | RPAREN
  | SEMICOLON
  (* Keywords *)
  | LET
  (* Misc *)
  | EOF

type t = {
  kind : token_kind;
  loc : Location.t;
}

let make kind loc = { kind; loc }

and name_of_token_kind = function
  | INT _ -> "number"
  | IDENT _ -> "identifier"
  | PLUS -> "plus sign"
  | MINUS -> "minus sign"
  | MULT -> "multiplication sign"
  | DIV -> "division sign"
  | EQ -> "equal sign"
  | LPAREN -> "left parenthesis"
  | RPAREN -> "right parenthesis"
  | SEMICOLON -> "semicolon"
  | LET -> "'let'"
  | EOF -> "end of file"

and keyword_of_string_opt = function "let" -> Some LET | _ -> None

and token_kind_loose_equal t1 t2 =
  match (t1, t2) with
  | INT _, INT _ -> true
  | IDENT _, IDENT _ -> true
  | _ -> t1 = t2
