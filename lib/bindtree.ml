type binop =
  | Plus
  | Minus
  | Mult
  | Div

type expr =
  | Int of int64
  | Var of string * int
  | BinOp of binop * expr * expr

type t =
  | Let of string * int * expr
  | Expr of expr
