type t = { input : string; pos : int }

let rec make (input : string) : t = { input; pos = 0 }
and advance (lexer : t) : t = { lexer with pos = lexer.pos + 1 }

and is_ident_char c =
  match c with 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true | _ -> false

and lex_integer (lexer : t) : Token.typ * t =
  let rec aux lexer acc =
    match String.get lexer.input lexer.pos with
    | '0' .. '9' as c -> aux (advance lexer) (String.make 1 c |> String.cat acc)
    | _ -> (Token.Integer acc, lexer)
  in
  aux lexer ""

and lex_identifier (lexer : t) : Token.typ * t =
  let rec aux lexer acc =
    match String.get lexer.input lexer.pos with
    | c when is_ident_char c ->
        aux (advance lexer) (String.make 1 c |> String.cat acc)
    | _ -> (
        match Token.to_keyword acc with
        | Some t -> (t, lexer)
        | _ -> (Token.Ident acc, lexer))
  in
  aux lexer ""

and next (lexer : t) : (Token.typ * t, string) result =
  if lexer.pos >= String.length lexer.input then Ok (EOF, lexer)
  else
    match String.get lexer.input lexer.pos with
    | ' ' | '\n' -> advance lexer |> next
    | '0' .. '9' -> Ok (lex_integer lexer)
    | '+' -> Ok (Plus, advance lexer)
    | '-' -> Ok (Minus, advance lexer)
    | '*' -> Ok (Mul, advance lexer)
    | '/' -> Ok (Div, advance lexer)
    | '%' -> Ok (Mod, advance lexer)
    | ';' -> Ok (SemiColon, advance lexer)
    | c when is_ident_char c -> Ok (lex_identifier lexer)
    | c -> Error (Printf.sprintf "unknown character '%c'" c)
