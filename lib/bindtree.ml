type binop =
  | Plus
  | Minus
  | Mult
  | Div

type expr =
  | Int of int64 * Location.t
  | Var of string * int * Location.t
  | BinOp of binop * expr * expr * Location.t

type t =
  | Let of string * int * expr * Location.t
  | Print of expr * Location.t
