let bytecode_of_binop = function
  | Bindtree.Plus -> Bytecode.ADD
  | Bindtree.Minus -> Bytecode.SUB
  | Bindtree.Mult -> Bytecode.MULT
  | Bindtree.Div -> Bytecode.DIV
  | Bindtree.Mod -> Bytecode.MOD
  | Bindtree.Eq -> Bytecode.CEQ
  | Bindtree.Neq -> Bytecode.CNE
  | Bindtree.Lt -> Bytecode.CLT
  | Bindtree.Le -> Bytecode.CLE
  | Bindtree.Gt -> Bytecode.CGT
  | Bindtree.Ge -> Bytecode.CGE

let rec compile_expr expr addr =
  match expr with
  | Bindtree.Int (n, _) -> ([ Bytecode.PUSH n ], addr + 1)
  | Bindtree.Var (_, id, _) -> ([ Bytecode.LOAD id ], addr + 1)
  | Bindtree.BinOp (op, lhs, rhs, _) ->
      let op = bytecode_of_binop op in
      let l, addr = compile_expr lhs addr in
      let r, addr = compile_expr rhs addr in
      (l @ r @ [ op ], addr + 1)

let rec compile_stmt stmt addr =
  match stmt with
  | Bindtree.Let (_, id, expr, _) ->
      let expr, addr = compile_expr expr addr in
      (expr @ [ Bytecode.STORE id ], addr + 1)
  | Bindtree.Print (expr, _) ->
      let expr, addr = compile_expr expr addr in
      (expr @ [ Bytecode.PRINT ], addr + 1)
  | Bindtree.PrintInt (expr, _) ->
      let expr, addr = compile_expr expr addr in
      (expr @ [ Bytecode.PRINT_INT ], addr + 1)
  | Bindtree.Assign (_, id, expr, _) ->
      let expr, addr = compile_expr expr addr in
      (expr @ [ Bytecode.STORE id ], addr + 1)
  | Bindtree.IfElse (cond, btrue, bfalse, _) ->
      let cond, jaddr = compile_expr cond addr in
      let bfalse, taddr =
        match bfalse with
        | None -> ([], jaddr + 2)
        | Some stmts ->
            let bfalse, taddr = compile_stmts stmts (jaddr + 1) in
            (bfalse, taddr + 1)
      in
      let btrue, eaddr = compile_stmts btrue taddr in
      ( cond
        @ [ Bytecode.JNZ (Int64.of_int taddr) ]
        @ bfalse
        @ [ Bytecode.JMP (Int64.of_int eaddr) ]
        @ btrue,
        eaddr )
  | Bindtree.While (cond, body, _) ->
      let cond, jaddr = compile_expr cond addr in
      let taddr = jaddr + 2 in
      let body, eaddr = compile_stmts body taddr in
      let eaddr = eaddr + 1 in
      ( cond
        @ [
            Bytecode.JNZ (Int64.of_int taddr);
            Bytecode.JMP (Int64.of_int eaddr);
          ]
        @ body
        @ [ Bytecode.JMP (Int64.of_int addr) ],
        eaddr )

and compile_stmts stmts addr =
  match stmts with
  | [] -> ([], addr)
  | stmt :: tl ->
      let code, addr = compile_stmt stmt addr in
      let r, addr = compile_stmts tl addr in
      (code @ r, addr)

let compile program vpc =
  let insts, _ = compile_stmts program 0 in
  Bytecode.make vpc (insts @ [ Bytecode.HALT ])
