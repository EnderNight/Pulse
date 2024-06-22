open Tokenizer

type operator =
  | Plus
  | Minus
  | Mult
  | Div
  | Lt
  | Le
  | Gt
  | Ge
  | Eq
  | Assign
  | Diff
[@@deriving show]

type expr =
  | Int of int
  | String of string
  | Var of string
  | Call of string * expr list
  | BinOp of operator * expr * expr
[@@deriving show]

type stmt =
  | Let of string * string * expr
  | If of expr * stmt list * stmt list option
  | While of expr * stmt list
  | Assign of string * expr
  | Return of expr
  | Expr of expr
[@@deriving show]

type param = { name : string; type_id : string } [@@deriving show]

type fun_dec = {
  name : string;
  params : param list;
  return_type : string;
  body : stmt list;
}
[@@deriving show]

type ast = fun_dec list [@@deriving show]

let rec parse_expr tokens = parse_assign tokens

and parse_assign tokens =
  let eq, tokens = parse_eq tokens in
  match tokens with
  | ASSIGN :: tokens ->
      let assign, tokens = parse_eq tokens in
      (BinOp (Assign, assign, assign), tokens)
  | _ -> (eq, tokens)

and parse_eq tokens =
  let rela, tokens = parse_relat tokens in
  match tokens with
  | EQUAL :: tokens ->
      let eq, tokens = parse_eq tokens in
      (BinOp (Eq, rela, eq), tokens)
  | DIFF :: tokens ->
      let eq, tokens = parse_eq tokens in
      (BinOp (Diff, rela, eq), tokens)
  | _ -> (rela, tokens)

and parse_relat tokens =
  let add, tokens = parse_addi tokens in
  match tokens with
  | LT :: tokens ->
      let rela, tokens = parse_relat tokens in
      (BinOp (Lt, add, rela), tokens)
  | LE :: tokens ->
      let rela, tokens = parse_relat tokens in
      (BinOp (Le, add, rela), tokens)
  | GT :: tokens ->
      let rela, tokens = parse_relat tokens in
      (BinOp (Gt, add, rela), tokens)
  | GE :: tokens ->
      let rela, tokens = parse_relat tokens in
      (BinOp (Ge, add, rela), tokens)
  | _ -> (add, tokens)

and parse_addi tokens =
  let multi, tokens = parse_multi tokens in
  match tokens with
  | PLUS :: tokens ->
      let add, tokens = parse_addi tokens in
      (BinOp (Plus, multi, add), tokens)
  | MINUS :: tokens ->
      let add, tokens = parse_addi tokens in
      (BinOp (Minus, multi, add), tokens)
  | _ -> (multi, tokens)

and parse_multi tokens =
  let postfix, tokens = parse_postfix tokens in
  match tokens with
  | MULT :: tokens ->
      let mult, tokens = parse_multi tokens in
      (BinOp (Mult, postfix, mult), tokens)
  | DIV :: tokens ->
      let mult, tokens = parse_multi tokens in
      (BinOp (Div, postfix, mult), tokens)
  | _ -> (postfix, tokens)

and parse_postfix tokens =
  let primary, rest = parse_primary tokens in
  match rest with
  | LPAREN :: rest -> (
      match primary with
      | Var name ->
          let rec parse_args tokens =
            let arg, rest = parse_primary tokens in
            match rest with
            | COMA :: rest ->
                let args, rest = parse_args rest in
                (arg :: args, rest)
            | RPAREN :: rest -> ([ arg ], rest)
            | _ ->
                failwith
                  ("Syntax error: expected '" ^ string_of_token RPAREN
                 ^ "'")
          in
          let args, rest = parse_args rest in
          (Call (name, args), rest)
      | _ ->
          failwith "Syntax error: wrong identifier for function call")
  | _ -> (primary, rest)

and parse_primary = function
  | IDENTIFIER id :: tokens -> (Var id, tokens)
  | INT_LITERAL i :: tokens -> (Int i, tokens)
  | STR_LITERAL s :: tokens -> (String s, tokens)
  | LPAREN :: tokens -> (
      let expr, tokens = parse_expr tokens in
      match tokens with
      | RPAREN :: tokens -> (expr, tokens)
      | _ -> failwith "Syntax error: unmatched parenthesis")
  | _ -> failwith "primary: Unreachable"

and parse_var_stmt tokens =
  match tokens with
  | LET
    :: IDENTIFIER name
    :: COLON
    :: IDENTIFIER type_id
    :: ASSIGN :: rest -> (
      let expr, rest = parse_expr rest in
      match rest with
      | SEMICOLON :: rest -> (Let (name, type_id, expr), rest)
      | _ -> failwith "Syntax error: missing ';'")
  | _ -> failwith "Unreachable var"

and parse_if_stmt tokens =
  match tokens with
  | IF :: LPAREN :: rest -> (
      let cond, rest = parse_expr rest in
      match rest with
      | RPAREN :: rest -> (
          let stmts, rest = parse_comp rest in
          match rest with
          | ELSE :: rest ->
              let else_stmts, rest = parse_comp rest in
              (If (cond, stmts, Some else_stmts), rest)
          | _ -> (If (cond, stmts, None), rest))
      | _ -> failwith "Syntax error: missing ') {'")
  | _ -> failwith "Unreachable if"

and parse_while_stmt tokens =
  match tokens with
  | WHILE :: LPAREN :: rest -> (
      let cond, rest = parse_expr rest in
      match rest with
      | RPAREN :: rest ->
          let stmts, rest = parse_comp rest in
          (While (cond, stmts), rest)
      | _ -> failwith "Syntax error")
  | _ -> failwith "Syntax error: missing '('"

and parse_return_stmt tokens =
  match tokens with
  | RETURN :: rest -> (
      let expr, rest = parse_expr rest in
      match rest with
      | SEMICOLON :: rest -> (Return expr, rest)
      | _ -> failwith "Syntax error: missing ;")
  | _ -> failwith "Unreachable return"

and parse_expr_stmt tokens =
  let expr, rest = parse_expr tokens in
  match rest with
  | SEMICOLON :: rest -> (Expr expr, rest)
  | token :: _ ->
      failwith
        ("Syntax error: unexpected '" ^ string_of_token token ^ "'")
  | _ -> failwith "Unreachable"

and parse_stmt tokens =
  match tokens with
  | IF :: _ -> parse_if_stmt tokens
  | LET :: _ -> parse_var_stmt tokens
  | RETURN :: _ -> parse_return_stmt tokens
  | WHILE :: _ -> parse_while_stmt tokens
  | _ -> parse_expr_stmt tokens

and parse_comp tokens =
  match tokens with
  | LBRACK :: body ->
      let rec parse_stmts tokens =
        let stmt, rest = parse_stmt tokens in
        match rest with
        | RBRACK :: rest -> ([ stmt ], rest)
        | _ ->
            let stmts, rest = parse_stmts rest in
            (stmt :: stmts, rest)
      in
      parse_stmts body
  | _ ->
      failwith
        ("Syntax error: expected '" ^ string_of_token LBRACK ^ "'")

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
              | token :: _ ->
                  failwith
                    ("Syntax error: unexpected token '"
                   ^ string_of_token token ^ "'")
              | _ -> failwith "Unreachable"
            in
            let params, rest = parse_params_tail rest in
            ({ name = id; type_id = param_type } :: params, rest)
        | _ -> failwith "Syntax error"
      in
      let params, tokens = parse_params rest in
      match tokens with
      | COLON :: IDENTIFIER return_type :: rest ->
          let body, rest = parse_comp rest in
          ({ name; params; return_type; body }, rest)
      | _ -> failwith "Syntax error")
  | _ -> failwith "Syntax error"

and parse tokens =
  match tokens with
  | LET :: _ ->
      let fun_dec, rest = parse_fun_dec tokens in
      fun_dec :: parse rest
  | [ EOF ] -> []
  | _ -> failwith "Syntax error"
