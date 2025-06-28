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

and skip_whitespaces lexer =
  match next_char lexer with
  | Some c, lexer when is_whitespace c -> skip_whitespaces lexer
  | _ -> lexer

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
        | '0' .. '9' ->
            let num, next_lexer = seek lexer is_number in
            Ok (Token.INT num, next_lexer)
        | c ->
            let msg = "Unexpected character '" ^ String.make 1 c ^ "'" in
            Error (Report.make loc msg))
  in
  let* ttype, lexer = get_ttype lexer in
  Ok (Token.make ttype loc, lexer)
