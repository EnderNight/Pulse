open Tokenizer

type operator = Plus | Minus | Mult | Div | Lt | Le | Gt | Ge

type expr =
  | Int of int
  | String of string
  | Var of string
  | Call of string * expr list
  | BinOp of operator * expr * expr

type stmt =
  | Let of string * string * expr
  | If of expr * stmt list * stmt list option
  | While of expr * stmt list
  | Assign of string * expr
  | Return of expr

type param = { name : string; type_id : string }

type fun_dec = {
  name : string;
  params : param list;
  return_type : string;
  body : stmt list;
}

type ast = fun_dec list

let rec parse_atom_expr tokens =
  match tokens with
  | INT_LITERAL i :: rest -> (Int i, rest)
  | STR_LITERAL s :: rest -> (String s, rest)
  | IDENTIFIER id :: rest -> (Var id, rest)
  | LPAREN :: rest -> parse_expr rest
  | _ -> failwith "Syntax error: atom_expr"

and parse_factor_expr tokens =
  let atom, tokens = parse_atom_expr tokens in
  match tokens with
  | MULT :: rest ->
      let factor, tokens = parse_factor_expr rest in
      (BinOp (Mult, atom, factor), tokens)
  | DIV :: rest ->
      let factor, tokens = parse_factor_expr rest in
      (BinOp (Div, atom, factor), tokens)
  | _ -> (atom, tokens)

and parse_expr tokens =
  match tokens with
  | IDENTIFIER name :: LPAREN :: rest ->
      let rec parse_params tokens =
        match tokens with
        | RPAREN :: rest -> ([], rest)
        | _ ->
            let parse_params_tail tokens =
              match tokens with
              | COMA :: rest -> parse_params rest
              | RPAREN :: rest -> ([], rest)
              | _ -> failwith "Syntax error: incorrect function call"
            in
            let param, rest = parse_expr tokens in
            let params, rest = parse_params_tail rest in
            (param :: params, rest)
      in
      let params, rest = parse_params rest in
      (Call (name, params), rest)
  | _ -> (
      let factor, rest = parse_factor_expr tokens in
      match rest with
      | PLUS :: rest ->
          let expr, tokens = parse_expr rest in
          (BinOp (Plus, factor, expr), tokens)
      | MINUS :: rest ->
          let expr, tokens = parse_factor_expr rest in
          (BinOp (Minus, factor, expr), tokens)
      | _ -> (factor, rest))

and parse_var_stmt tokens =
  match tokens with
  | LET
    :: IDENTIFIER name
    :: COLON
    :: IDENTIFIER type_id
    :: EQUAL :: rest -> (
      let expr, rest = parse_expr rest in
      match rest with
      | SEMICOLON :: rest -> (Let (name, type_id, expr), rest)
      | _ -> failwith "Syntax error: missing ';'")
  | _ -> failwith "Unreachable"

and parse_assign_stmt tokens =
  match tokens with
  | IDENTIFIER id :: EQUAL :: rest -> (
      let expr, rest = parse_expr rest in
      match rest with
      | SEMICOLON :: rest -> (Assign (id, expr), rest)
      | _ -> failwith "Syntax error: missing semicolon ';'")
  | _ -> failwith "Unreachable"

and parse_if_stmt tokens =
  match tokens with
  | IF :: LPAREN :: rest -> (
      let cond, rest = parse_expr rest in
      match rest with
      | RPAREN :: LBRACK :: rest -> (
          let stmts, rest = parse_stmts rest in
          match rest with
          | RBRACK :: ELSE :: LBRACK :: rest -> (
              let else_stmts, rest = parse_stmts rest in
              match rest with
              | RBRACK :: rest ->
                  (If (cond, stmts, Some else_stmts), rest)
              | _ -> failwith "Syntax error: missing '}'")
          | RBRACK :: rest -> (If (cond, stmts, None), rest)
          | _ -> failwith "Syntax error: missing '}'")
      | _ -> failwith "Syntax error: missing ') {'")
  | _ -> failwith "Unreachable"

and parse_while_stmt tokens =
  match tokens with
  | WHILE :: LPAREN :: rest -> (
      let cond, rest = parse_expr rest in
      match rest with
      | RPAREN :: LBRACK :: rest -> (
          let stmts, rest = parse_stmts rest in
          match rest with
          | RBRACK :: rest -> (While (cond, stmts), rest)
          | _ -> failwith "Syntax error")
      | _ -> failwith "Syntax error")
  | _ -> failwith "Syntax error: missing '('"

and parse_return_stmt tokens =
  match tokens with
  | RETURN :: rest -> (
      let expr, rest = parse_expr rest in
      match rest with
      | SEMICOLON :: rest -> (Return expr, rest)
      | _ -> failwith "Syntax error: missing ;")
  | _ -> failwith "Unreachable"

and parse_stmts tokens =
  match tokens with
  | IDENTIFIER _ :: _ ->
      let assign, rest = parse_assign_stmt tokens in
      let stmts, rest = parse_stmts rest in
      (assign :: stmts, rest)
  | IF :: _ ->
      let if_block, rest = parse_if_stmt tokens in
      let stmts, rest = parse_stmts rest in
      (if_block :: stmts, rest)
  | LET :: _ ->
      let var, rest = parse_var_stmt tokens in
      let stmts, rest = parse_stmts rest in
      (var :: stmts, rest)
  | RETURN :: _ ->
      let ret, rest = parse_return_stmt tokens in
      let stmts, rest = parse_stmts rest in
      (ret :: stmts, rest)
  | WHILE :: _ ->
      let while_block, rest = parse_while_stmt tokens in
      let stmts, rest = parse_stmts rest in
      (while_block :: stmts, rest)
  | _ -> ([], tokens)

and parse_fun_dec tokens =
  match tokens with
  | LET :: IDENTIFIER name :: LPAREN :: rest -> (
      let rec parse_params tokens =
        match tokens with
        | RPAREN :: rest -> ([], rest)
        | IDENTIFIER id :: COLON :: IDENTIFIER param_type :: rest ->
            let parse_params_tail tokens =
              match tokens with
              | COMA :: rest -> parse_params rest
              | RPAREN :: rest -> ([], rest)
              | _ -> failwith "Syntax error"
            in
            let params, rest = parse_params_tail rest in
            ({ name = id; type_id = param_type } :: params, rest)
        | _ -> failwith "Syntax error"
      in
      let params, tokens = parse_params rest in
      match tokens with
      | COLON :: IDENTIFIER return_type :: LBRACK :: rest -> (
          let body, rest = parse_stmts rest in
          match rest with
          | RBRACK :: rest ->
              ({ name; params; return_type; body }, rest)
          | _ -> failwith "Syntax error")
      | _ -> failwith "Syntax error")
  | _ -> failwith "Syntax error"

and parse tokens =
  match tokens with
  | LET :: _ ->
      let fun_dec, rest = parse_fun_dec tokens in
      fun_dec :: parse rest
  | [ EOF ] -> []
  | _ -> failwith "Syntax error"
