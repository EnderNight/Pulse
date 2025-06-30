type t = {
  program : string;
  pos : int;
  loc : Location.t;
}

let rec make source program =
  let loc = Location.make source in
  { program; pos = 0; loc }

and is_whitespace = function ' ' | '\t' | '\n' -> true | _ -> false
and is_newline = function '\n' -> true | _ -> false
and is_number = function '0' .. '9' -> true | _ -> false

and is_identifier = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false

and is_identifier_idx c idx =
  match c with
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | '0' .. '9' when idx <> 0 -> true
  | _ -> false

and next_char lexer =
  if lexer.pos >= String.length lexer.program then (None, lexer)
  else
    let c = String.get lexer.program lexer.pos in
    let loc =
      if is_newline c then Location.advance_line lexer.loc
      else Location.advance_col lexer.loc
    in
    (Some c, { lexer with pos = lexer.pos + 1; loc })

and seek lexer predicate =
  let pos = lexer.pos in
  let rec aux lexer acc =
    match next_char lexer with
    | Some c, lexer when predicate c -> aux lexer (acc + 1)
    | _ -> (acc, lexer)
  in
  let len, lexer = aux lexer 0 in
  (String.sub lexer.program pos len, lexer)

and seeki lexer predicate =
  let pos = lexer.pos in
  let rec aux lexer acc =
    match next_char lexer with
    | Some c, lexer when predicate c acc -> aux lexer (acc + 1)
    | _ -> (acc, lexer)
  in
  let len, lexer = aux lexer 0 in
  (String.sub lexer.program pos len, lexer)

and skip_whitespaces lexer =
  match next_char lexer with
  | Some c, lexer when is_whitespace c -> skip_whitespaces lexer
  | _ -> lexer

and lex_number lexer = seek lexer is_number
and lex_identifier lexer = seeki lexer is_identifier_idx

and next_token lexer =
  let ( let* ) = Result.bind in
  let lexer = skip_whitespaces lexer in
  let loc = lexer.loc in
  let get_ttype lexer =
    match next_char lexer with
    | None, lexer -> Ok (Token.EOF, lexer)
    | Some c, next_lexer -> (
        match c with
        | '+' -> Ok (Token.PLUS, next_lexer)
        | '-' -> Ok (Token.MINUS, next_lexer)
        | '*' -> Ok (Token.MULT, next_lexer)
        | '/' -> Ok (Token.DIV, next_lexer)
        | '(' -> Ok (Token.LPAREN, next_lexer)
        | ')' -> Ok (Token.RPAREN, next_lexer)
        | ';' -> Ok (Token.SEMICOLON, next_lexer)
        | '=' -> Ok (Token.EQ, next_lexer)
        | '0' .. '9' ->
            let num, next_lexer = lex_number lexer in
            Ok (Token.INT num, next_lexer)
        | c when is_identifier c -> (
            let ident, next_lexer = lex_identifier lexer in
            match Token.keyword_of_string_opt ident with
            | Some kind -> Ok (kind, next_lexer)
            | _ -> Ok (Token.IDENT ident, next_lexer))
        | c ->
            let msg = "unknown character '" ^ String.make 1 c ^ "'" in
            Error (Report.make_loc loc msg))
  in
  let* ttype, lexer = get_ttype lexer in
  Ok (Token.make ttype loc, lexer)
