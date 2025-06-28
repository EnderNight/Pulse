type token_type =
  (* Constants *)
  | INT of string
  (* Operators *)
  | PLUS
  | MINUS
  | MULT
  | DIV
  (* Punctuators *)
  | LPAREN
  | RPAREN
  (* Misc *)
  | EOF

type t = {
  ttype : token_type;
  loc : Location.t;
}

let make ttype loc = { ttype; loc }
