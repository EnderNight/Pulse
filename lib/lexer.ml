type t = {
  program : string;
  pos : int;
  loc : Location.t;
}

let rec make program source =
  let loc = Location.make source in
  { program; pos = 0; loc }

and is_whitespace = function ' ' | '\t' | '\n' -> true | _ -> false
and is_newline = function '\n' -> true | _ -> false

and next_char lexer =
  if lexer.pos >= String.length lexer.program then (None, lexer)
  else
    let c = String.get lexer.program lexer.pos in
    let loc =
      if is_newline c then Location.advance_line lexer.loc
      else Location.advance_col lexer.loc
    in
    (Some c, { lexer with pos = lexer.pos + 1; loc })

and lex_number lexer =
  let pos = lexer.pos - 1 in
  let rec aux lexer acc =
    match next_char lexer with
    | Some '0' .. '9', lexer -> aux lexer (acc + 1)
    | _ -> (acc, lexer)
  in
  let len, lexer = aux lexer 1 in
  (String.sub lexer.program pos len, lexer)

and skip_whitespaces lexer =
  match next_char lexer with
  | Some c, lexer when is_whitespace c -> skip_whitespaces lexer
  | _ -> lexer

and next_token lexer =
  let ( let* ) = Result.bind in
  let lexer = skip_whitespaces lexer in
  let loc = lexer.loc in
  let rec get_ttype lexer =
    match next_char lexer with
    | None, lexer -> Ok (Token.EOF, lexer)
    | Some c, lexer -> (
        match c with
        | c when is_whitespace c -> get_ttype lexer
        | '+' -> Ok (Token.PLUS, lexer)
        | '-' -> Ok (Token.MINUS, lexer)
        | '*' -> Ok (Token.MULT, lexer)
        | '/' -> Ok (Token.DIV, lexer)
        | '0' .. '9' ->
            let num, lexer = lex_number lexer in
            Ok (Token.INT num, lexer)
        | c ->
            let msg = "Unknown character '" ^ String.make 1 c ^ "'" in
            Error (Report.make loc msg))
  in
  let* ttype, lexer = get_ttype lexer in
  Ok (Token.make ttype loc, lexer)
