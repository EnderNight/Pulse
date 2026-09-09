type value = Var of string | Integer of int64

type instruction =
  | Print of value
  | Add of string * value * value
  | Sub of string * value * value

type var_generator = { acc : int }

let gen_var (generator : var_generator) : string * var_generator =
  (Printf.sprintf "v%d" generator.acc, { acc = generator.acc + 1 })

let rec flatten_expr (expr : Ast.expr) (g : var_generator) :
    value * instruction list * var_generator =
  match expr with
  | Integer i ->
      let var, g = gen_var g in
      (Integer i, [], g)
  | BinExpr (op, l, r) -> (
      let vlhs, lhs, g = flatten_expr l g in
      let vrhs, rhs, g = flatten_expr r g in
      let v, g = gen_var g in
      match op with
      | Plus ->
          (Var v, List.append (List.append lhs rhs) [ Add (v, vlhs, vrhs) ], g)
      | Minus ->
          (Var v, List.append (List.append lhs rhs) [ Sub (v, vlhs, vrhs) ], g))

and flatten_stmt (stmt : Ast.stmt) (g : var_generator) :
    instruction list * var_generator =
  match stmt with
  | Print expr ->
      let v, insts, g = flatten_expr expr g in
      (List.append insts [ Print v ], g)

and flatten (tree : Ast.t) : instruction list =
  let rec aux tree g acc =
    match tree with
    | [] -> (acc, g)
    | stmt :: tl ->
        let insts, g = flatten_stmt stmt g in
        aux tl g (List.append acc insts)
  in
  let insts, _ = aux tree { acc = 0 } [] in
  insts
