module StringMap = Map.Make (String)

type generator = Types.ty StringMap.t list

let ( let* ) = Result.bind

let rec gen_empty = [ StringMap.empty ]
and gen_pop_scope g = List.tl g
and gen_push_scope g = StringMap.empty :: g

and gen_find_opt v g =
  match g with
  | [] -> None
  | tmap :: g -> (
      match StringMap.find_opt v tmap with
      | None -> gen_find_opt v g
      | Some ty -> Some ty)

and gen_add v ty g =
  match g with
  | [] -> failwith "gen_add: empty scope map"
  | tmap :: g ->
      let tmap = StringMap.add v ty tmap in
      tmap :: g

let is_assignable (expr : Astree.expression) =
  match expr.kind with
  | Astree.ArrayAccess _ | Astree.Ident _ -> true
  | _ -> false

and get_var_type v loc g =
  match gen_find_opt v g with
  | Some ty -> Ok ty
  | None ->
      let msg = "undefined variable '" ^ v ^ "'." in
      let report = Report.make_loc loc msg in
      Error report

and gen_expr ty kind loc : Typetree.expression = { ty; kind; loc }
and gen_stmt kind loc : Typetree.statement = { kind; loc }

and expect ety aty loc =
  match Types.is_compatible ety aty with
  | true -> Ok aty
  | false ->
      let msg = "invalid type. Expected '" ^ ety.name ^ "'." in
      let report = Report.make_loc loc msg in
      Error report

let rec check_expr (expr : Astree.expression) g =
  match expr.kind with
  | Astree.Number n ->
      let kind = Typetree.Number n in
      let expr = gen_expr Types.primitive_int kind expr.loc in
      Ok (expr, g)
  | Astree.Ident id ->
      let* ty = get_var_type id expr.loc g in
      let kind = Typetree.Var id in
      let expr = gen_expr ty kind expr.loc in
      Ok (expr, g)
  | Astree.BinOp (binop, lhs, rhs) ->
      let* lhs, g = check_expr lhs g in
      let* rhs, g = check_expr rhs g in
      let* _ = expect Types.primitive_int lhs.ty lhs.loc in
      let* _ = expect Types.primitive_int rhs.ty rhs.loc in
      let kind = Typetree.BinOp (binop, lhs, rhs) in
      let expr = gen_expr lhs.ty kind expr.loc in
      Ok (expr, g)
  | Astree.ArrayInit expr ->
      let* expr, g = check_expr expr g in
      let* _ = expect Types.primitive_int expr.ty expr.loc in
      let kind = Typetree.ArrayInit expr in
      let expr = gen_expr Types.primitive_array kind expr.loc in
      Ok (expr, g)
  | Astree.ArrayAccess (var, index) ->
      let* var, g = check_expr var g in
      let* _ = expect Types.primitive_array var.ty expr.loc in
      let* index, g = check_expr index g in
      let* _ = expect Types.primitive_int index.ty expr.loc in
      let kind = Typetree.ArrayAccess (var, index) in
      let expr = gen_expr Types.primitive_int kind expr.loc in
      Ok (expr, g)

and check_stmt (stmt : Astree.statement) g =
  match stmt.kind with
  | Astree.Let (id, expr) ->
      let* expr, g = check_expr expr g in
      let g = gen_add id expr.ty g in
      let kind = Typetree.Let (id, expr) in
      let stmt = gen_stmt kind stmt.loc in
      Ok (stmt, g)
  | Astree.If (cond, btrue) ->
      let* cond, g = check_expr cond g in
      let* _ = expect Types.primitive_int cond.ty cond.loc in
      let* btrue, g = check_block btrue g in
      let kind = Typetree.If (cond, btrue) in
      let stmt = gen_stmt kind stmt.loc in
      Ok (stmt, g)
  | Astree.IfElse (cond, btrue, bfalse) ->
      let* cond, g = check_expr cond g in
      let* _ = expect Types.primitive_int cond.ty cond.loc in
      let* btrue, g = check_block btrue g in
      let* bfalse, g = check_block bfalse g in
      let kind = Typetree.IfElse (cond, btrue, bfalse) in
      let stmt = gen_stmt kind stmt.loc in
      Ok (stmt, g)
  | Astree.While (cond, body) ->
      let* cond, g = check_expr cond g in
      let* _ = expect Types.primitive_int cond.ty cond.loc in
      let* body, g = check_block body g in
      let kind = Typetree.While (cond, body) in
      let stmt = gen_stmt kind stmt.loc in
      Ok (stmt, g)
  | Astree.Assign (var, expr) ->
      if is_assignable var then
        let* var, g = check_expr var g in
        let* expr, g = check_expr expr g in
        let* _ = expect var.ty expr.ty expr.loc in
        let kind = Typetree.Assign (var, expr) in
        let stmt = gen_stmt kind stmt.loc in
        Ok (stmt, g)
      else
        let msg = "expression not assignable" in
        let report = Report.make_loc var.loc msg in
        Error report
  | Astree.Expression expr ->
      let* expr, g = check_expr expr g in
      let kind = Typetree.Expression expr in
      let stmt = gen_stmt kind stmt.loc in
      Ok (stmt, g)
  | Astree.Print expr ->
      let* expr, g = check_expr expr g in
      let* _ = expect Types.primitive_int expr.ty expr.loc in
      let kind = Typetree.Print expr in
      let stmt = gen_stmt kind stmt.loc in
      Ok (stmt, g)
  | Astree.PrintInt expr ->
      let* expr, g = check_expr expr g in
      let* _ = expect Types.primitive_int expr.ty expr.loc in
      let kind = Typetree.PrintInt expr in
      let stmt = gen_stmt kind stmt.loc in
      Ok (stmt, g)

and check_block block g =
  let rec aux block acc g =
    match block with
    | [] -> Ok (List.rev acc, g)
    | stmt :: tl ->
        let* stmt, g = check_stmt stmt g in
        aux tl (stmt :: acc) g
  in
  let g = gen_push_scope g in
  let* block, g = aux block [] g in
  let g = gen_pop_scope g in
  Ok (block, g)

and type_check program =
  let rec aux stmts acc g =
    match stmts with
    | [] -> Ok (List.rev acc)
    | stmt :: stmts ->
        let* stmt, g = check_stmt stmt g in
        aux stmts (stmt :: acc) g
  in
  aux program [] gen_empty
