type t =
  | Number of int64
  | Plus of t * t
  | Minus of t * t
  | Mult of t * t
  | Div of t * t
[@@deriving show]
