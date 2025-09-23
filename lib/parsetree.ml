type binop =
  | Plus
  | Minus
  | Mult
  | Div
  | Mod
  | Eq
  | Neq
  | Lt
  | Le
  | Gt
  | Ge

type expr =
  | Int of int64 * Location.t
  | Var of string * Location.t
  | BinOp of binop * expr * expr * Location.t

type statement =
  | Let of string * expr * Location.t
  | Print of expr * Location.t
  | IfElse of expr * statement list * statement list option * Location.t

type t = statement list
