type t =
  (* Constants *)
  | NUMBER of string
  (* Punctuators *)
  | PLUS
  | MINUS
  | MULT
  | DIV
  | LPAREN
  | RPAREN
[@@deriving show]
