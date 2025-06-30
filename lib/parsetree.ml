type binop =
  | Plus
  | Minus
  | Mult
  | Div

type expr =
  | Int of int64 * Location.t
  | Var of string * Location.t
  | BinOp of binop * expr * expr * Location.t

type t =
  | Let of string * expr * Location.t
  | Print of expr * Location.t
