module StringMap = Map.Make (String)

type generator = {
  op_map : Ir.operand StringMap.t;
  ident_acc : int;
}

let init = { op_map = StringMap.empty; ident_acc = 0 }

and gen_ident g ident =
  let new_id = ident ^ "_" ^ string_of_int g.ident_acc in
  ({ g with ident_acc = g.ident_acc + 1 }, new_id)

and get_op_opt g ident =
  match StringMap.find_opt ident g.op_map with
  | Some op -> Some op
  | _ -> None

and add_op g ident op =
  let op_map = StringMap.add ident op g.op_map in
  { g with op_map }

let rec gen_ir_expr g expr =
  match expr with
  | Parsetree.Int n -> (g, [], Ir.Const n)
  | Parsetree.Var ident -> (
      match get_op_opt g ident with
      | None -> failwith "get_ir_expr: undeclared variable"
      | Some op -> (g, [], op))
  | Parsetree.BinOp (binop, lhs, rhs) ->
      let g, linsts, lop = gen_ir_expr g lhs in
      let g, rinsts, rop = gen_ir_expr g rhs in
      let g, id = gen_ident g "" in
      let inst =
        match binop with
        | Parsetree.Plus -> Ir.Add (id, lop, rop)
        | Parsetree.Minus -> Ir.Sub (id, lop, rop)
        | Parsetree.Mult -> Ir.Mul (id, lop, rop)
        | Parsetree.Div -> Ir.Div (id, lop, rop)
      in
      (g, linsts @ rinsts @ [ inst ], Ir.Var id)

and gen_ir_stmt g stmt =
  match stmt with
  | Parsetree.Let (ident, expr) ->
      let g, insts, op = gen_ir_expr g expr in
      let g = add_op g ident op in
      (g, insts)
  | Parsetree.Print expr ->
      let g, insts, op = gen_ir_expr g expr in
      (g, insts @ [ Ir.Print op ])

and gen_ir_program g program =
  match program with
  | [] -> []
  | stmt :: tl ->
      let g, insts = gen_ir_stmt g stmt in
      insts @ gen_ir_program g tl

and gen_ir (program : Parsetree.program) =
  let g = init in
  gen_ir_program g program
