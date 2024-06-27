(* AST *)
type bin_op =
  | Plus
  | Minus
  | Mult
  | Div
  | Lt
  | Le
  | Gt
  | Ge
  | Eq
  | Neq
  | Assign
[@@deriving show]

and expr =
  | Int of int
  | Str of string
  | BinOp of bin_op * expr * expr
  | Var of string
  | Call of string * expr list
[@@deriving show]

and stmt =
  | Let of var_dec
  | If of expr * stmt list * stmt list option
  | While of expr * stmt list
  | Return of expr option
  | Expr of expr
[@@deriving show]

and var_dec = { name : string; type_id : string; value : expr option }
[@@deriving show]

and fun_dec = {
  name : string;
  params : var_dec list;
  type_id : string;
  body : stmt list;
}
[@@deriving show]

and decl = VarDec of var_dec | FunDec of fun_dec [@@deriving show]
and t = decl list [@@deriving show]
