let ( let* ) = Result.bind

let expect lexer token_kind =
  let* token, lexer = Lexer.next_token lexer in
  match Token.token_kind_loose_equal token.kind token_kind with
  | true -> Ok (token, lexer)
  | false ->
      let msg =
        "unexpected character. Expecting "
        ^ Token.name_of_token_kind token_kind
        ^ "."
      in
      Error (Report.make_loc token.loc msg)

and expect_ident lexer =
  let* token, lexer = Lexer.next_token lexer in
  match token.kind with
  | Token.IDENT id -> Ok (token, id, lexer)
  | _ ->
      let msg =
        "unexpected character. Expecting "
        ^ Token.name_of_token_kind token.kind
        ^ "."
      in
      Error (Report.make_loc token.loc msg)

let gen_expr kind loc : Astree.expression = { kind; loc }
and gen_stmt kind loc : Astree.statement = { kind; loc }

let rec parse_primary_expr lexer =
  let* token, lexer = Lexer.next_token lexer in
  match token.kind with
  | Token.INT n ->
      let kind = Astree.Number (Int64.of_string n) in
      let expr = gen_expr kind token.loc in
      Ok (expr, lexer)
  | Token.IDENT id ->
      let kind = Astree.Ident id in
      let expr = gen_expr kind token.loc in
      Ok (expr, lexer)
  | Token.LBRACK ->
      let* expr, lexer = parse_expr lexer in
      let* _, lexer = expect lexer Token.RBRACK in
      let kind = Astree.ArrayInit expr in
      let expr = gen_expr kind token.loc in
      Ok (expr, lexer)
  | Token.LPAREN ->
      let* expr, lexer = parse_expr lexer in
      let* _, lexer = expect lexer Token.RPAREN in
      Ok (expr, lexer)
  | _ ->
      let msg = "unexpected character." in
      Error (Report.make_loc token.loc msg)

and parse_postfix_expr lexer =
  let* primary, lexer = parse_primary_expr lexer in
  let* token, nlexer = Lexer.next_token lexer in
  match token.kind with
  | Token.LBRACK ->
      let* expr, lexer = parse_expr nlexer in
      let* _, lexer = expect lexer RBRACK in
      let kind = Astree.ArrayAccess (primary, expr) in
      let expr = gen_expr kind primary.loc in
      Ok (expr, lexer)
  | _ -> Ok (primary, lexer)

and parse_mult_expr lexer =
  let* postfix, lexer = parse_postfix_expr lexer in
  let rec aux lexer tacc =
    let* token, nlexer = Lexer.next_token lexer in
    match token.kind with
    | Token.MULT ->
        let* postfix, lexer = parse_postfix_expr nlexer in
        let kind = Astree.BinOp (Astree.Mul, tacc, postfix) in
        let expr = gen_expr kind token.loc in
        aux lexer expr
    | Token.DIV ->
        let* postfix, lexer = parse_postfix_expr nlexer in
        let kind = Astree.BinOp (Astree.Div, tacc, postfix) in
        let expr = gen_expr kind token.loc in
        aux lexer expr
    | Token.MOD ->
        let* postfix, lexer = parse_postfix_expr nlexer in
        let kind = Astree.BinOp (Astree.Mod, tacc, postfix) in
        let expr = gen_expr kind token.loc in
        aux lexer expr
    | _ -> Ok (tacc, lexer)
  in
  aux lexer postfix

and parse_add_expr lexer =
  let* mult, lexer = parse_mult_expr lexer in
  let rec aux lexer tacc =
    let* token, nlexer = Lexer.next_token lexer in
    match token.kind with
    | Token.PLUS ->
        let* mult, lexer = parse_mult_expr nlexer in
        let kind = Astree.BinOp (Astree.Add, tacc, mult) in
        let expr = gen_expr kind token.loc in
        aux lexer expr
    | Token.MINUS ->
        let* mult, lexer = parse_mult_expr nlexer in
        let kind = Astree.BinOp (Astree.Sub, tacc, mult) in
        let expr = gen_expr kind token.loc in
        aux lexer expr
    | _ -> Ok (tacc, lexer)
  in
  aux lexer mult

and parse_shift_expr lexer =
  let* add, lexer = parse_add_expr lexer in
  let rec aux lexer tacc =
    let* token, nlexer = Lexer.next_token lexer in
    match token.kind with
    | Token.SHL ->
        let* add, lexer = parse_add_expr nlexer in
        let kind = Astree.BinOp (Astree.Shl, tacc, add) in
        let expr = gen_expr kind token.loc in
        aux lexer expr
    | Token.SHR ->
        let* add, lexer = parse_add_expr nlexer in
        let kind = Astree.BinOp (Astree.Shr, tacc, add) in
        let expr = gen_expr kind token.loc in
        aux lexer expr
    | _ -> Ok (tacc, lexer)
  in
  aux lexer add

and parse_rela_expr lexer =
  let* shift, lexer = parse_shift_expr lexer in
  let rec aux lexer tacc =
    let* token, nlexer = Lexer.next_token lexer in
    match token.kind with
    | Token.LT ->
        let* shift, lexer = parse_shift_expr nlexer in
        let kind = Astree.BinOp (Astree.Lt, tacc, shift) in
        let expr = gen_expr kind token.loc in
        aux lexer expr
    | Token.LE ->
        let* shift, lexer = parse_shift_expr nlexer in
        let kind = Astree.BinOp (Astree.Le, tacc, shift) in
        let expr = gen_expr kind token.loc in
        aux lexer expr
    | Token.GT ->
        let* shift, lexer = parse_shift_expr nlexer in
        let kind = Astree.BinOp (Astree.Gt, tacc, shift) in
        let expr = gen_expr kind token.loc in
        aux lexer expr
    | Token.GE ->
        let* shift, lexer = parse_shift_expr nlexer in
        let kind = Astree.BinOp (Astree.Ge, tacc, shift) in
        let expr = gen_expr kind token.loc in
        aux lexer expr
    | _ -> Ok (tacc, lexer)
  in
  aux lexer shift

and parse_equal_expr lexer =
  let* rela, lexer = parse_rela_expr lexer in
  let rec aux lexer tacc =
    let* token, nlexer = Lexer.next_token lexer in
    match token.kind with
    | Token.DEQ ->
        let* rela, lexer = parse_rela_expr nlexer in
        let kind = Astree.BinOp (Astree.Eq, tacc, rela) in
        let expr = gen_expr kind token.loc in
        aux lexer expr
    | Token.NEQ ->
        let* rela, lexer = parse_rela_expr nlexer in
        let kind = Astree.BinOp (Astree.Neq, tacc, rela) in
        let expr = gen_expr kind token.loc in
        aux lexer expr
    | _ -> Ok (tacc, lexer)
  in
  aux lexer rela

and parse_and_expr lexer =
  let* equal, lexer = parse_equal_expr lexer in
  let rec aux lexer tacc =
    let* token, nlexer = Lexer.next_token lexer in
    match token.kind with
    | Token.AND ->
        let* equal, lexer = parse_equal_expr nlexer in
        let kind = Astree.BinOp (Astree.And, tacc, equal) in
        let expr = gen_expr kind token.loc in
        aux lexer expr
    | _ -> Ok (tacc, lexer)
  in
  aux lexer equal

and parse_or_expr lexer =
  let* and_expr, lexer = parse_and_expr lexer in
  let rec aux lexer tacc =
    let* token, nlexer = Lexer.next_token lexer in
    match token.kind with
    | Token.OR ->
        let* and_expr, lexer = parse_and_expr nlexer in
        let kind = Astree.BinOp (Astree.Or, tacc, and_expr) in
        let expr = gen_expr kind token.loc in
        aux lexer expr
    | _ -> Ok (tacc, lexer)
  in
  aux lexer and_expr

and parse_expr lexer = parse_or_expr lexer

and parse_let_stmt lexer =
  let* token, lexer = expect lexer Token.LET in
  let* _, id, lexer = expect_ident lexer in
  let* _, lexer = expect lexer Token.EQ in
  let* expr, lexer = parse_expr lexer in
  let* _, lexer = expect lexer Token.SEMICOLON in
  let kind = Astree.Let (id, expr) in
  let stmt = gen_stmt kind token.loc in
  Ok (stmt, lexer)

and parse_if_stmt lexer =
  let* token, lexer = expect lexer Token.IF in
  let* _, lexer = expect lexer Token.LPAREN in
  let* cond, lexer = parse_expr lexer in
  let* _, lexer = expect lexer Token.RPAREN in
  let* btrue, lexer = parse_block lexer in
  let* etoken, elexer = Lexer.next_token lexer in
  match etoken.kind with
  | Token.ELSE ->
      let* bfalse, lexer = parse_block elexer in
      let kind = Astree.IfElse (cond, btrue, bfalse) in
      let stmt = gen_stmt kind token.loc in
      Ok (stmt, lexer)
  | _ ->
      let kind = Astree.If (cond, btrue) in
      let stmt = gen_stmt kind token.loc in
      Ok (stmt, lexer)

and parse_while_stmt lexer =
  let* token, lexer = expect lexer Token.WHILE in
  let* _, lexer = expect lexer Token.LPAREN in
  let* cond, lexer = parse_expr lexer in
  let* _, lexer = expect lexer Token.RPAREN in
  let* block, lexer = parse_block lexer in
  let kind = Astree.While (cond, block) in
  let stmt = gen_stmt kind token.loc in
  Ok (stmt, lexer)

and parse_print_stmt lexer =
  let* token, lexer = expect lexer Token.PRINT in
  let* expr, lexer = parse_expr lexer in
  let* _, lexer = expect lexer Token.SEMICOLON in
  let kind = Astree.Print expr in
  let stmt = gen_stmt kind token.loc in
  Ok (stmt, lexer)

and parse_print_int_stmt lexer =
  let* token, lexer = expect lexer Token.PRINT_INT in
  let* expr, lexer = parse_expr lexer in
  let* _, lexer = expect lexer Token.SEMICOLON in
  let kind = Astree.PrintInt expr in
  let stmt = gen_stmt kind token.loc in
  Ok (stmt, lexer)

and parse_postfix_stmt lexer =
  let* expr, lexer = parse_expr lexer in
  let* token, alexer = Lexer.next_token lexer in
  let* kind, lexer =
    match token.kind with
    | Token.EQ ->
        let* val_expr, lexer = parse_expr alexer in
        let kind = Astree.Assign (expr, val_expr) in
        Ok (kind, lexer)
    | _ ->
        let kind = Astree.Expression expr in
        Ok (kind, lexer)
  in
  let* _, lexer = expect lexer Token.SEMICOLON in
  let stmt = gen_stmt kind expr.loc in
  Ok (stmt, lexer)

and parse_statement lexer =
  let* token, _ = Lexer.next_token lexer in
  match token.kind with
  | Token.LET -> parse_let_stmt lexer
  | Token.IF -> parse_if_stmt lexer
  | Token.WHILE -> parse_while_stmt lexer
  | Token.PRINT -> parse_print_stmt lexer
  | Token.PRINT_INT -> parse_print_int_stmt lexer
  | _ -> parse_postfix_stmt lexer

and parse_block lexer =
  let* _, lexer = expect lexer Token.LBRACE in
  let* block, lexer =
    let rec aux lexer sacc =
      let* token, nlexer = Lexer.next_token lexer in
      match token.kind with
      | Token.RBRACE -> Ok (List.rev sacc, nlexer)
      | _ ->
          let* stmt, lexer = parse_statement lexer in
          aux lexer (stmt :: sacc)
    in
    aux lexer []
  in
  Ok (block, lexer)

and parse lexer =
  let rec aux lexer sacc =
    let* token, _ = Lexer.next_token lexer in
    match token.kind with
    | Token.EOF -> Ok (List.rev sacc, lexer)
    | _ ->
        let* stmt, lexer = parse_statement lexer in
        aux lexer (stmt :: sacc)
  in
  let* prog, _ = aux lexer [] in
  Ok prog
