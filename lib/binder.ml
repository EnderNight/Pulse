let ( let* ) = Result.bind

let rec bind_expr expr env =
  match expr with
  | Parsetree.Int (num, loc) -> Ok (Bindtree.Int (num, loc))
  | Parsetree.Var (ident, loc) -> (
      match Hashtbl.find_opt env ident with
      | None ->
          Error
            (Report.make_loc loc ("undeclared variable '" ^ ident ^ "'."))
      | Some id -> Ok (Bindtree.Var (ident, id, loc)))
  | Parsetree.BinOp (binop, lhs, rhs, loc) ->
      let* l = bind_expr lhs env in
      let* r = bind_expr rhs env in
      let binop_inst =
        match binop with
        | Parsetree.Plus -> Bindtree.Plus
        | Parsetree.Minus -> Bindtree.Minus
        | Parsetree.Mult -> Bindtree.Mult
        | Parsetree.Div -> Bindtree.Div
        | Parsetree.Mod -> Bindtree.Mod
        | Parsetree.Eq -> Bindtree.Eq
        | Parsetree.Neq -> Bindtree.Neq
        | Parsetree.Lt -> Bindtree.Lt
        | Parsetree.Le -> Bindtree.Le
        | Parsetree.Gt -> Bindtree.Gt
        | Parsetree.Ge -> Bindtree.Ge
      in
      Ok (Bindtree.BinOp (binop_inst, l, r, loc))

and bind_statement stmt env acc =
  match stmt with
  | Parsetree.Let (ident, expr, loc) ->
      Hashtbl.add env ident acc;
      let* expr = bind_expr expr env in
      Ok (Bindtree.Let (ident, acc, expr, loc), acc + 1)
  | Parsetree.Print (expr, loc) ->
      let* expr = bind_expr expr env in
      Ok (Bindtree.Print (expr, loc), acc)
  | Parsetree.PrintInt (expr, loc) ->
      let* expr = bind_expr expr env in
      Ok (Bindtree.PrintInt (expr, loc), acc)
  | Parsetree.Assign (ident, expr, loc) -> (
      let* expr = bind_expr expr env in
      match Hashtbl.find_opt env ident with
      | None ->
          Error
            (Report.make_loc loc ("undeclared variable '" ^ ident ^ "'."))
      | Some id -> Ok (Bindtree.Assign (ident, id, expr, loc), acc))
  | Parsetree.IfElse (cond, btrue, bfalse, loc) -> (
      let* cond = bind_expr cond env in
      let* btrue, acc = bind_statements btrue env acc in
      match bfalse with
      | None -> Ok (Bindtree.IfElse (cond, btrue, None, loc), acc)
      | Some bfalse ->
          let* bfalse, acc = bind_statements bfalse env acc in
          Ok (Bindtree.IfElse (cond, btrue, Some bfalse, loc), acc))
  | Parsetree.While (cond, body, loc) ->
      let* cond = bind_expr cond env in
      let* body, acc = bind_statements body env acc in
      Ok (Bindtree.While (cond, body, loc), acc)

and bind_statements stmts env acc =
  let rec aux stmts id acc =
    match stmts with
    | [] -> Ok (List.rev acc, id)
    | tree :: tl ->
        let* tree, id = bind_statement tree env id in
        aux tl id (tree :: acc)
  in
  aux stmts acc []

and bind program =
  let env = Hashtbl.create 8 in
  bind_statements program env 0
