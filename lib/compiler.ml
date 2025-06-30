let rec compile_expr expr =
  match expr with
  | Bindtree.Int (n, _) -> [ Bytecode.PUSH n ]
  | Bindtree.Var (_, id, _) -> [ Bytecode.LOAD id ]
  | Bindtree.BinOp (binop, lhs, rhs, _) ->
      let l = compile_expr lhs
      and r = compile_expr rhs
      and binop_inst =
        match binop with
        | Bindtree.Plus -> Bytecode.ADD
        | Bindtree.Minus -> Bytecode.SUB
        | Bindtree.Mult -> Bytecode.MULT
        | Bindtree.Div -> Bytecode.DIV
      in
      l @ r @ [ binop_inst ]

and compile_tree tree =
  match tree with
  | Bindtree.Let (_, id, expr, _) ->
      let e = compile_expr expr in
      e @ [ Bytecode.STORE id ]
  | Bindtree.Expr expr -> compile_expr expr

and compile trees variable_pool_count =
  let instructions =
    (List.map compile_tree trees |> List.flatten) @ [ Bytecode.HALT ]
  in
  Bytecode.make variable_pool_count instructions
