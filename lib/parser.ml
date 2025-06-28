let ( let* ) = Result.bind

let rec parse_primary lexer =
  let* token, lexer = Lexer.next_token lexer in
  match token.ttype with
  | Token.INT n -> Ok (Parsetree.Int (Int64.of_string n), lexer)
  | Token.LPAREN -> (
      let* expr, lexer = parse_expr lexer in
      let* token, lexer = Lexer.next_token lexer in
      match token.ttype with
      | Token.RPAREN -> Ok (expr, lexer)
      | _ ->
          Error
            (Report.make token.loc
               "Unexpected token. Expecting a closing parenthesis"))
  | _ ->
      Error
        (Report.make token.loc
           "Unexpected token. Expecting a number or a left parenthesis")

and parse_factor lexer =
  let* primary, lexer = parse_primary lexer in
  let rec aux lexer tree =
    let* token, next_lexer = Lexer.next_token lexer in
    match token.ttype with
    | Token.MULT ->
        let* primary, lexer = parse_primary next_lexer in
        aux lexer (Parsetree.Mult (tree, primary))
    | Token.DIV ->
        let* primary, lexer = parse_primary next_lexer in
        aux lexer (Parsetree.Div (tree, primary))
    | _ -> Ok (tree, lexer)
  in
  aux lexer primary

and parse_term lexer =
  let* factor, lexer = parse_factor lexer in
  let rec aux lexer tree =
    let* token, next_lexer = Lexer.next_token lexer in
    match token.ttype with
    | Token.PLUS ->
        let* factor, lexer = parse_factor next_lexer in
        aux lexer (Parsetree.Plus (tree, factor))
    | Token.MINUS ->
        let* factor, lexer = parse_factor next_lexer in
        aux lexer (Parsetree.Minus (tree, factor))
    | _ -> Ok (tree, lexer)
  in
  aux lexer factor

and parse_expr lexer = parse_term lexer

and parse_program lexer =
  let* expr, lexer = parse_expr lexer in
  let* token, _ = Lexer.next_token lexer in
  if token.ttype <> Token.EOF then
    Error
      (Report.make token.loc "Unexpected token. Expecting end of file")
  else Ok expr

and parse lexer = parse_program lexer
