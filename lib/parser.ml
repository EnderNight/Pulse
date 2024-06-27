open Lexer
open Error
open Parsetree
open Token

(* Parser *)
type t = { lexer : Lexer.t }

let rec make lexer = { lexer }

and make_loc parser =
  Location.make parser.lexer.input_name parser.lexer.line
    parser.lexer.col

and advance parser =
  match lex parser.lexer with
  | Error e -> Error e
  | Ok (token, lexer) -> Ok (token, { lexer })

and advance_n parser n =
  match n with
  | n when n <= 1 -> (
      match advance parser with
      | Error e -> Error e
      | Ok (token, parser) -> Ok ([ token ], parser))
  | _ -> (
      match advance parser with
      | Error e -> Error e
      | Ok (token, parser) -> (
          match advance_n parser (n - 1) with
          | Error e -> Error e
          | Ok (tokens, parser) -> Ok (token :: tokens, parser)))

and parse_primary_expr parser =
  match advance parser with
  | Error e -> Error e
  | Ok (token, parser) -> (
      match token.kind with
      | INT_CONST i -> Ok (Int i, parser)
      | STR_LIT s -> Ok (Str s, parser)
      | ID id -> Ok (Var id, parser)
      | LPAREN -> (
          match parse_expr parser with
          | Error e -> Error e
          | Ok (exp, parser) -> (
              match advance parser with
              | Ok (token, parser) when token.kind = RPAREN ->
                  Ok (exp, parser)
              | Ok (token, _) ->
                  Error
                    {
                      loc = token.loc;
                      msg =
                        "unexpected token '" ^ show_token_value token
                        ^ "'. Expecting a closing ')'";
                    }
              | Error e -> Error e))
      | _ ->
          Error
            {
              loc = token.loc;
              msg =
                "unexpected token '" ^ show_token_value token ^ "'";
            })

and parse_args parser =
  match advance parser with
  | Error e -> Error e
  | Ok (token, parser) when token.kind = RPAREN -> Ok ([], parser)
  | _ -> (
      match parse_expr parser with
      | Error e -> Error e
      | Ok (exp, parser) -> (
          match advance parser with
          | Error e -> Error e
          | Ok (token, parser) when token.kind = RPAREN ->
              Ok ([ exp ], parser)
          | Ok (token, parser) when token.kind = COMMA -> (
              match parse_args parser with
              | Error e -> Error e
              | Ok (exps, parser) -> Ok (exp :: exps, parser))
          | Ok (token, _) ->
              Error
                {
                  loc = token.loc;
                  msg =
                    "unexpected token '" ^ show_token_value token
                    ^ "'";
                }))

and parse_postfix_expr parser =
  match parse_primary_expr parser with
  | Error e -> Error e
  | Ok (Var id, parser) -> (
      match advance parser with
      | Error e -> Error e
      | Ok (token, parser) when token.kind = LPAREN -> (
          match parse_args parser with
          | Error e -> Error e
          | Ok (args, parser) -> Ok (Call (id, args), parser))
      | _ -> Ok (Var id, parser))
  | Ok (exp, parser) -> Ok (exp, parser)

and parse_mult_expr parser =
  match parse_postfix_expr parser with
  | Error e -> Error e
  | Ok (exp, parser) -> (
      match advance parser with
      | Error e -> Error e
      | Ok (token, parser) when token.kind = MULT || token.kind = DIV
        -> (
          match parse_mult_expr parser with
          | Error e -> Error e
          | Ok (exp2, parser) ->
              let op = if token.kind = MULT then Mult else Div in
              Ok (BinOp (op, exp, exp2), parser))
      | _ -> Ok (exp, parser))

and parse_add_expr parser =
  match parse_mult_expr parser with
  | Error e -> Error e
  | Ok (exp, parser) -> (
      match advance parser with
      | Error e -> Error e
      | Ok (token, parser)
        when token.kind = PLUS || token.kind = MINUS -> (
          match parse_add_expr parser with
          | Error e -> Error e
          | Ok (exp2, parser) ->
              let op = if token.kind = PLUS then Plus else Minus in
              Ok (BinOp (op, exp, exp2), parser))
      | _ -> Ok (exp, parser))

and parse_rela_expr parser =
  match parse_add_expr parser with
  | Error e -> Error e
  | Ok (exp, parser) -> (
      match advance parser with
      | Error e -> Error e
      | Ok (token, parser)
        when token.kind = LT || token.kind = LE || token.kind = GT
             || token.kind = GE -> (
          match parse_rela_expr parser with
          | Error e -> Error e
          | Ok (exp2, parser) ->
              let op =
                match token.kind with
                | LT -> Lt
                | LE -> Le
                | GT -> Gt
                | GE -> Ge
                | _ -> failwith "uncreachable"
              in
              Ok (BinOp (op, exp, exp2), parser))
      | _ -> Ok (exp, parser))

and parse_equ_expr parser =
  match parse_rela_expr parser with
  | Error e -> Error e
  | Ok (exp, parser) -> (
      match advance parser with
      | Error e -> Error e
      | Ok (token, parser)
        when token.kind = EQUAL || token.kind = NEQUAL -> (
          match parse_equ_expr parser with
          | Error e -> Error e
          | Ok (exp2, parser) ->
              let op = if token.kind = EQUAL then Eq else Neq in
              Ok (BinOp (op, exp, exp2), parser))
      | _ -> Ok (exp, parser))

and parse_assign_expr parser =
  match parse_postfix_expr parser with
  | Error e -> Error e
  | Ok (exp, assign_parser) -> (
      match advance assign_parser with
      | Error e -> Error e
      | Ok (token, parser) when token.kind = ASSIGN -> (
          match parse_equ_expr parser with
          | Error e -> Error e
          | Ok (value, parser) ->
              Ok (BinOp (Assign, exp, value), parser))
      | _ -> parse_equ_expr parser)

and parse_expr parser = parse_assign_expr parser

and parse_var_dec parser =
  match advance_n parser 5 with
  | Error e -> Error e
  | Ok (tokens, parser) -> (
      match get_kind_list tokens with
      | [ LET; ID name; COLON; ID type_id; ASSIGN ] -> (
          match parse_expr parser with
          | Error e -> Error e
          | Ok (expr, parser) -> (
              match advance parser with
              | Error e -> Error e
              | Ok (token, parser) when token.kind = SEMICOLON ->
                  Ok ({ name; type_id; value = Some expr }, parser)
              | Ok (token, _) ->
                  Error
                    {
                      loc = token.loc;
                      msg =
                        "unexpected token '" ^ show_token_value token
                        ^ "'";
                    }))
      | _ ->
          Error
            {
              loc = make_loc parser;
              msg = "incorrect variable definition syntax";
            })

and parse_params parser =
  match advance parser with
  | Error e -> Error e
  | Ok (token, parser) when token.kind = RPAREN -> Ok ([], parser)
  | _ -> (
      match advance_n parser 3 with
      | Error e -> Error e
      | Ok (tokens, parser) -> (
          match get_kind_list tokens with
          | [ ID name; COLON; ID type_id ] -> (
              let var = { name; type_id; value = None } in
              match advance parser with
              | Error e -> Error e
              | Ok (token, parser) when token.kind = RPAREN ->
                  Ok ([ var ], parser)
              | Ok (token, parser) when token.kind = COMMA -> (
                  match parse_params parser with
                  | Error e -> Error e
                  | Ok (params, parser) -> Ok (var :: params, parser))
              | Ok (token, _) ->
                  Error
                    {
                      loc = token.loc;
                      msg =
                        "unexpected token '" ^ show_token_value token
                        ^ "'";
                    })
          | _ ->
              Error
                {
                  loc = make_loc parser;
                  msg = "incorrect parameters definition syntax";
                }))

and parse_stmt parser =
  match advance parser with
  | Error e -> Error e
  | Ok (token, _) -> (
      match token.kind with
      | LET -> (
          match parse_var_dec parser with
          | Error e -> Error e
          | Ok (dec, parser) -> Ok (Let dec, parser))
      | IF -> parse_if parser
      | RETURN -> parse_return parser
      | WHILE -> parse_while parser
      | _ -> parse_expr_stmt parser)

and parse_if parser =
  match advance_n parser 2 with
  | Error e -> Error e
  | Ok (tokens, parser) -> (
      match get_kind_list tokens with
      | [ IF; LPAREN ] -> (
          match parse_expr parser with
          | Error e -> Error e
          | Ok (cond, parser) -> (
              match advance_n parser 2 with
              | Error e -> Error e
              | Ok (tokens, parser) -> (
                  match get_kind_list tokens with
                  | [ RPAREN; LBRACK ] -> (
                      match parse_stmts parser with
                      | Error e -> Error e
                      | Ok (stmts, parser) -> (
                          match advance parser with
                          | Error e -> Error e
                          | Ok (token, _) when token.kind = ELSE -> (
                              match parse_else parser with
                              | Error e -> Error e
                              | Ok (else_stmts, parser) ->
                                  Ok
                                    ( If (cond, stmts, Some else_stmts),
                                      parser ))
                          | _ -> Ok (If (cond, stmts, None), parser)))
                  | _ ->
                      Error
                        {
                          loc = make_loc parser;
                          msg = "incorrect if statment syntax";
                        })))
      | _ ->
          Error
            {
              loc = make_loc parser;
              msg = "incorrect if statment syntax";
            })

and parse_else parser =
  match advance_n parser 2 with
  | Error e -> Error e
  | Ok (tokens, parser) -> (
      match get_kind_list tokens with
      | [ ELSE; LBRACK ] -> parse_stmts parser
      | _ ->
          Error
            {
              loc = make_loc parser;
              msg = "incorrect else statment syntax";
            })

and parse_return parser =
  match advance parser with
  | Error e -> Error e
  | Ok (token, parser) when token.kind = RETURN -> (
      match advance parser with
      | Error e -> Error e
      | Ok (token, parser) when token.kind = SEMICOLON ->
          Ok (Return None, parser)
      | _ -> (
          match parse_expr parser with
          | Error e -> Error e
          | Ok (exp, parser) -> (
              match advance parser with
              | Error e -> Error e
              | Ok (token, parser) when token.kind = SEMICOLON ->
                  Ok (Return (Some exp), parser)
              | _ ->
                  Error { loc = make_loc parser; msg = "missing ';'" }
              )))
  | _ ->
      Error
        {
          loc = make_loc parser;
          msg = "incorrect return statment syntax";
        }

and parse_while parser =
  match advance_n parser 2 with
  | Error e -> Error e
  | Ok (tokens, parser) -> (
      match get_kind_list tokens with
      | [ WHILE; LPAREN ] -> (
          match parse_expr parser with
          | Error e -> Error e
          | Ok (cond, parser) -> (
              match advance_n parser 2 with
              | Error e -> Error e
              | Ok (tokens, parser) -> (
                  match get_kind_list tokens with
                  | [ RPAREN; LBRACK ] -> (
                      match parse_stmts parser with
                      | Error e -> Error e
                      | Ok (stmts, parser) ->
                          Ok (While (cond, stmts), parser))
                  | _ ->
                      Error
                        {
                          loc = make_loc parser;
                          msg =
                            "incorrect while statment syntax after \
                             condition";
                        })))
      | _ ->
          Error
            {
              loc = make_loc parser;
              msg = "incorrect while statment syntax before condition";
            })

and parse_expr_stmt parser =
  match parse_expr parser with
  | Error e -> Error e
  | Ok (exp, parser) -> (
      match advance parser with
      | Error e -> Error e
      | Ok (token, parser) when token.kind = SEMICOLON ->
          Ok (Expr exp, parser)
      | _ -> Error { loc = make_loc parser; msg = "missing ';'" })

and parse_stmts parser =
  match advance parser with
  | Error e -> Error e
  | Ok (token, parser) when token.kind = RBRACK -> Ok ([], parser)
  | _ -> (
      match parse_stmt parser with
      | Error e -> Error e
      | Ok (stmt, parser) -> (
          match parse_stmts parser with
          | Error e -> Error e
          | Ok (stmts, parser) -> Ok (stmt :: stmts, parser)))

and parse_fun_dec parser =
  match advance_n parser 3 with
  | Error e -> Error e
  | Ok (tokens, parser) -> (
      match get_kind_list tokens with
      | [ FUN; ID id; LPAREN ] -> (
          match parse_params parser with
          | Error e -> Error e
          | Ok (params, parser) -> (
              match advance_n parser 3 with
              | Error e -> Error e
              | Ok (tokens, parser) -> (
                  match get_kind_list tokens with
                  | [ COLON; ID type_id; LBRACK ] -> (
                      match parse_stmts parser with
                      | Error e -> Error e
                      | Ok (stmts, parser) ->
                          Ok
                            ( {
                                name = id;
                                params;
                                type_id;
                                body = stmts;
                              },
                              parser ))
                  | _ ->
                      Error
                        {
                          loc = make_loc parser;
                          msg =
                            "incorrect function definition syntax \
                             after params";
                        })))
      | _ ->
          Error
            {
              loc = make_loc parser;
              msg =
                "incorrect function definition syntax before params";
            })

and parse_decl parser =
  match advance parser with
  | Error e -> Error e
  | Ok (token, _) -> (
      match token.kind with
      | LET -> (
          match parse_var_dec parser with
          | Error e -> Error e
          | Ok (dec, parser) -> Ok (VarDec dec, parser))
      | FUN -> (
          match parse_fun_dec parser with
          | Error e -> Error e
          | Ok (dec, parser) -> Ok (FunDec dec, parser))
      | _ ->
          Error
            {
              loc = make_loc parser;
              msg = "incorrect declaration syntax";
            })

and parse_program parser =
  match parse_decl parser with
  | Error e -> Error e
  | Ok (decl, parser) -> (
      match advance parser with
      | Error e -> Error e
      | Ok (token, _) when token.kind = FUN || token.kind = LET -> (
          match parse_program parser with
          | Error e -> Error e
          | Ok decls -> Ok (decl :: decls))
      | _ -> Ok ([ decl ] : Parsetree.t))

and parse parser = parse_program parser
