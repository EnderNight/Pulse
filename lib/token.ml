type t =
  (* Constants *)
  | NUMBER of string
  (* Symbols *)
  | PLUS
  | MINUS
  | MULT
  | DIV
[@@deriving show]
