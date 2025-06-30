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
      in
      Ok (Bindtree.BinOp (binop_inst, l, r, loc))

and bind_tree tree env acc =
  match tree with
  | Parsetree.Let (ident, expr, loc) ->
      Hashtbl.add env ident acc;
      let* expr = bind_expr expr env in
      Ok (Bindtree.Let (ident, acc, expr, loc), acc + 1)
  | Parsetree.Expr expr ->
      let* expr = bind_expr expr env in
      Ok (Bindtree.Expr expr, acc)

and bind trees =
  let env = Hashtbl.create 8 in
  let rec aux trees id acc =
    match trees with
    | [] -> Ok (List.rev acc, id)
    | tree :: tl ->
        let* tree, id = bind_tree tree env id in
        aux tl id (tree :: acc)
  in
  aux trees 0 []
