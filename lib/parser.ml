let rec parse_primary tokens =
  match tokens with
  | Token.NUMBER num :: tl -> (Parsetree.Number (Int64.of_string num), tl)
  | Token.LPAREN :: tl -> (
      let tree, tokens = parse_expr tl in
      match tokens with
      | Token.RPAREN :: tl -> (tree, tl)
      | _ -> failwith "Expecting right parenthesis")
  | _ -> failwith "Unexpected token"

and parse_factor tokens =
  let primary, tokens = parse_primary tokens in
  let rec aux p tokens =
    match tokens with
    | Token.MULT :: tl ->
        let pr, tokens = parse_primary tl in
        aux (Parsetree.Mult (p, pr)) tokens
    | Token.DIV :: tl ->
        let pr, tokens = parse_primary tl in
        aux (Parsetree.Div (p, pr)) tokens
    | _ -> (p, tokens)
  in
  aux primary tokens

and parse_term tokens =
  let factor, tokens = parse_factor tokens in
  let rec aux f tokens =
    match tokens with
    | Token.PLUS :: tl ->
        let fa, tokens = parse_factor tl in
        aux (Parsetree.Plus (f, fa)) tokens
    | Token.MINUS :: tl ->
        let fa, tokens = parse_factor tl in
        aux (Parsetree.Minus (f, fa)) tokens
    | _ -> (f, tokens)
  in
  aux factor tokens

and parse_expr tokens = parse_term tokens

and parse_program tokens =
  let term, tokens = parse_expr tokens in
  if List.is_empty tokens then term else failwith "Expecting end of file"

and parse tokens = parse_program tokens
