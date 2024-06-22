type operator =
  | Plus
  | Minus
  | Mult
  | Div
  | Lt
  | Le
  | Gt
  | Ge
  | Eq
  | Assign
  | Diff
[@@deriving show]

type expr =
  | Int of int
  | String of string
  | Var of string
  | Call of string * expr list
  | BinOp of operator * expr * expr
[@@deriving show]

type stmt =
  | Let of string * string * expr
  | If of expr * stmt list * stmt list option
  | While of expr * stmt list
  | Assign of string * expr
  | Return of expr
  | Expr of expr
[@@deriving show]

type param = { name : string; type_id : string } [@@deriving show]

type fun_dec = {
  name : string;
  params : param list;
  return_type : string;
  body : stmt list;
}
[@@deriving show]

type ast = fun_dec list [@@deriving show]

val parse : Tokenizer.token list -> ast
