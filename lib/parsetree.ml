type binop =
  | Plus
  | Minus
  | Mult
  | Div

type expr =
  | Int of int64
  | Var of string
  | BinOp of binop * expr * expr

type stmt =
  | Let of string * expr
  | Print of expr

type program = stmt list
