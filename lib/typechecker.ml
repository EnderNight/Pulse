module StringMap = Map.Make (String)

let ( let* ) = Result.bind

let rec check_expr expr tmap =
  match expr with
  | Parsetree.Int (n, loc) ->
      let kind = Typetree.Int n in
      let expr : Typetree.expression =
        { ty = Types.primitive_int; kind; loc }
      in
      Ok (expr, tmap)
  | Parsetree.Var (id, loc) -> (
      let kind = Typetree.Var id in
      match StringMap.find_opt id tmap with
      | Some ty ->
          let expr : Typetree.expression = { ty; kind; loc } in
          Ok (expr, tmap)
      | None ->
          let report =
            Report.make_loc loc ("undefined variable '" ^ id ^ "'")
          in
          Error report)
  | Parsetree.BinOp (op, lhs, rhs, loc) ->
      let* lhs, tmap = check_expr lhs tmap in
      let* rhs, tmap = check_expr rhs tmap in
      if Types.is_compatible lhs.ty rhs.ty then
        let kind = Typetree.BinOp (op, lhs, rhs) in
        let expr : Typetree.expression = { ty = lhs.ty; kind; loc } in
        Ok (expr, tmap)
      else
        let report = Report.make_loc loc "type mismatch" in
        Error report

let rec check_statement stmt tmap =
  match stmt with
  | Parsetree.Let (id, expr, loc) ->
      let* expr, tmap = check_expr expr tmap in
      let tmap = StringMap.add id expr.ty tmap in
      let kind = Typetree.Let (id, expr) in
      let stmt : Typetree.statement = { kind; loc } in
      Ok (stmt, tmap)
  | Parsetree.Assign (id, expr, loc) -> (
      let* expr, tmap = check_expr expr tmap in
      match StringMap.find_opt id tmap with
      | Some ty ->
          if Types.is_compatible ty expr.ty then
            let kind = Typetree.Assign (id, expr) in
            let stmt : Typetree.statement = { kind; loc } in
            Ok (stmt, tmap)
          else
            let report = Report.make_loc loc "type mismatch" in
            Error report
      | None ->
          let report =
            Report.make_loc loc ("undefined variable '" ^ id ^ "'")
          in
          Error report)
  | Parsetree.IfElse (cond, btrue, bfalse, loc) ->
      let* cond, tmap = check_expr cond tmap in
      if Types.is_compatible cond.ty Types.primitive_int then
        let* btrue, tmap = check_statement_block btrue tmap in
        let* bfalse, tmap =
          match bfalse with
          | None -> Ok (None, tmap)
          | Some bfalse ->
              let* bfalse, tmap = check_statement_block bfalse tmap in
              Ok (Some bfalse, tmap)
        in
        let kind = Typetree.IfElse (cond, btrue, bfalse) in
        let stmt : Typetree.statement = { kind; loc } in
        Ok (stmt, tmap)
      else
        let report = Report.make_loc loc "type mismatch, expected 'int'" in
        Error report
  | Parsetree.While (cond, body, loc) ->
      let* cond, tmap = check_expr cond tmap in
      if Types.is_compatible cond.ty Types.primitive_int then
        let* body, tmap = check_statement_block body tmap in
        let kind = Typetree.While (cond, body) in
        let stmt : Typetree.statement = { kind; loc } in
        Ok (stmt, tmap)
      else
        let report = Report.make_loc loc "type mismatch, expected 'int'" in
        Error report
  | Parsetree.Print (expr, loc) ->
      let* expr, tmap = check_expr expr tmap in
      if Types.is_compatible expr.ty Types.primitive_int then
        let kind = Typetree.Print expr in
        let stmt : Typetree.statement = { kind; loc } in
        Ok (stmt, tmap)
      else
        let report = Report.make_loc loc "type mismatch, expected 'int'" in
        Error report
  | Parsetree.PrintInt (expr, loc) ->
      let* expr, tmap = check_expr expr tmap in
      if Types.is_compatible expr.ty Types.primitive_int then
        let kind = Typetree.PrintInt expr in
        let stmt : Typetree.statement = { kind; loc } in
        Ok (stmt, tmap)
      else
        let report = Report.make_loc loc "type mismatch, expected 'int'" in
        Error report

and check_statement_block block tmap =
  let rec aux block tmap acc =
    match block with
    | [] -> Ok (List.rev acc, tmap)
    | stmt :: tl ->
        let* stmt, tmap = check_statement stmt tmap in
        aux tl tmap (stmt :: acc)
  in
  aux block tmap []

let type_check program =
  Result.map fst (check_statement_block program StringMap.empty)
