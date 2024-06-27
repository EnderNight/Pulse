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

and expr =
  | Int of int
  | Str of string
  | BinOp of bin_op * expr * expr
  | Var of string
  | Call of string * expr list

and stmt =
  | Let of var_dec
  | If of expr * stmt list * stmt list option
  | While of expr * stmt list
  | Return of expr option
  | Expr of expr

and var_dec = { name : string; type_id : string; value : expr option }

and fun_dec = {
  name : string;
  params : var_dec list;
  type_id : string;
  body : stmt list;
}

and decl = VarDec of var_dec | FunDec of fun_dec
and t = decl list [@@deriving show]

let rec string_of_binop = function
  | Plus -> "+"
  | Minus -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Lt -> "<"
  | Le -> "<="
  | Gt -> ">"
  | Ge -> ">="
  | Eq -> "=="
  | Neq -> "!="
  | Assign -> "="

and code_expr tree =
  match tree with
  | Int i -> string_of_int i
  | Str s -> "\"" ^ s ^ "\""
  | Var v -> v
  | BinOp (op, lexp, rexp) ->
      "(" ^ code_expr lexp ^ " " ^ string_of_binop op ^ " "
      ^ code_expr rexp ^ ")"
  | Call (name, exps) ->
      let rec code_exps exps =
        match exps with
        | [] -> ""
        | exp :: [] -> code_expr exp
        | exp :: exps -> code_expr exp ^ ", " ^ code_exps exps
      in
      name ^ "(" ^ code_exps exps ^ ")"

and code_var_dec var_decl =
  var_decl.name ^ ": " ^ var_decl.type_id
  ^
  match var_decl.value with
  | None -> ""
  | Some value -> " = " ^ code_expr value

and code_var_decs var_decs =
  match var_decs with
  | [] -> ""
  | var_dec :: [] -> code_var_dec var_dec
  | var_dec :: var_decs ->
      code_var_dec var_dec ^ ", " ^ code_var_decs var_decs

and code_stmts stmts prefix =
  match stmts with
  | [] -> ""
  | stmt :: [] -> code_stmt stmt prefix
  | stmt :: stmts ->
      code_stmt stmt prefix ^ "\n" ^ code_stmts stmts prefix

and code_stmt stmt prefix =
  prefix
  ^
  match stmt with
  | Let v -> "let " ^ code_var_dec v ^ ";\n"
  | If (cond, stmts, else_block) -> (
      "if (" ^ code_expr cond ^ ") {\n"
      ^ code_stmts stmts (prefix ^ "    ")
      ^ prefix ^ "}\n"
      ^
      match else_block with
      | None -> ""
      | Some stmts ->
          prefix ^ "else {\n"
          ^ code_stmts stmts (prefix ^ "    ")
          ^ prefix ^ "}\n")
  | While (cond, stmts) ->
      "while (" ^ code_expr cond ^ ") {\n"
      ^ code_stmts stmts (prefix ^ "    ")
      ^ prefix ^ "}\n"
  | Return ret_exp -> (
      "return"
      ^
      match ret_exp with
      | None -> ";\n"
      | Some exp -> " " ^ code_expr exp ^ ";\n")
  | Expr exp -> code_expr exp ^ ";\n"

and code_fun_dec (fun_decl : fun_dec) =
  "fun " ^ fun_decl.name ^ "("
  ^ code_var_decs fun_decl.params
  ^ "): " ^ fun_decl.type_id ^ " {\n"
  ^ code_stmts fun_decl.body "    "
  ^ "}\n"

and code_decl decl =
  match decl with
  | VarDec v -> code_stmt (Let v) "    "
  | FunDec f -> code_fun_dec f

and code_decls decls =
  match decls with
  | [] -> ""
  | decl :: [] -> code_decl decl
  | decl :: decls -> code_decl decl ^ "\n" ^ code_decls decls

and code program = code_decls program
