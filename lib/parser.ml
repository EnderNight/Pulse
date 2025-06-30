open Token

let ( let* ) = Result.bind

let rec expect lexer kind =
  let* token, lexer = Lexer.next_token lexer in
  if token_kind_loose_equal token.kind kind then Ok (token, lexer)
  else
    Error
      (Report.make_loc token.loc
         ("expected " ^ name_of_token_kind kind ^ "."))

and expect_list lexer kinds =
  match kinds with
  | kind :: tl ->
      let* token, lexer = expect lexer kind in
      let* tokens, lexer = expect_list lexer tl in
      Ok (token :: tokens, lexer)
  | [] -> Ok ([], lexer)

and expect_or lexer kinds =
  let* token, lexer = Lexer.next_token lexer in
  let equals = List.map (token_kind_loose_equal token.kind) kinds in
  if Utils.any equals then Ok (token, lexer)
  else
    let msg =
      "expected "
      ^ String.concat " or " (List.map name_of_token_kind kinds)
      ^ "."
    in
    Error (Report.make_loc token.loc msg)

and parse_primary lexer =
  let* token, lexer = expect_or lexer [ INT ""; IDENT ""; LPAREN ] in
  match token.kind with
  | INT num -> Ok (Parsetree.Int (Int64.of_string num, token.loc), lexer)
  | IDENT id -> Ok (Parsetree.Var (id, token.loc), lexer)
  | _ ->
      let* expr, lexer = parse_expr lexer in
      let* _, lexer = expect lexer RPAREN in
      Ok (expr, lexer)

and parse_factor lexer =
  let* primary, lexer = parse_primary lexer in
  let rec aux lexer tree =
    let* token, next_lexer = Lexer.next_token lexer in
    match token.kind with
    | MULT ->
        let* primary, lexer = parse_primary next_lexer in
        aux lexer
          (Parsetree.BinOp (Parsetree.Mult, tree, primary, token.loc))
    | DIV ->
        let* primary, lexer = parse_primary next_lexer in
        aux lexer
          (Parsetree.BinOp (Parsetree.Div, tree, primary, token.loc))
    | _ -> Ok (tree, lexer)
  in
  aux lexer primary

and parse_term lexer =
  let* factor, lexer = parse_factor lexer in
  let rec aux lexer tree =
    let* token, next_lexer = Lexer.next_token lexer in
    match token.kind with
    | PLUS ->
        let* factor, lexer = parse_factor next_lexer in
        aux lexer
          (Parsetree.BinOp (Parsetree.Plus, tree, factor, token.loc))
    | MINUS ->
        let* factor, lexer = parse_factor next_lexer in
        aux lexer
          (Parsetree.BinOp (Parsetree.Minus, tree, factor, token.loc))
    | _ -> Ok (tree, lexer)
  in
  aux lexer factor

and parse_expr lexer = parse_term lexer

and parse_statment lexer =
  let* token, lexer = expect_or lexer [ LET; PRINT ] in
  match token.kind with
  | LET -> (
      let* tokens, lexer = expect_list lexer [ IDENT ""; EQ ] in
      match List.nth tokens 0 with
      | { kind = IDENT ident; loc } ->
          let* expr, lexer = parse_expr lexer in
          let* _, lexer = expect lexer SEMICOLON in
          Ok (Parsetree.Let (ident, expr, loc), lexer)
      | _ -> failwith "parse_statment: Unreachable")
  | _ ->
      let* expr, lexer = parse_expr lexer in
      let* _, lexer = expect lexer SEMICOLON in
      Ok (Parsetree.Print (expr, token.loc), lexer)

and parse_program lexer =
  let rec aux lexer acc =
    let* stmt, lexer = parse_statment lexer in
    let* token, _ = Lexer.next_token lexer in
    if token.kind <> EOF then aux lexer (stmt :: acc)
    else Ok (stmt :: acc, lexer)
  in
  let* trees, _ = aux lexer [] in
  Ok (List.rev trees)

and parse lexer = parse_program lexer
