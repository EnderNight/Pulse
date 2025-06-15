type t = { source : string; pos : int; cur : char option }

let rec is_number = function '0' .. '9' -> true | _ -> false
and is_whitespace = function ' ' | '\n' -> true | _ -> false
and init source = { source; pos = 0; cur = Some (String.get source 0) }

and advance lexer =
  let new_pos = lexer.pos + 1 in
  if new_pos >= String.length lexer.source then { lexer with cur = None }
  else
    { lexer with pos = new_pos; cur = Some (String.get lexer.source new_pos) }

and lex_number lexer =
  let pos = lexer.pos in
  let rec aux lexer acc =
    match lexer.cur with
    | Some c when is_number c -> aux (advance lexer) (acc + 1)
    | _ -> (Token.NUMBER (String.sub lexer.source pos acc), lexer)
  in
  aux lexer 0

and lex lexer =
  match lexer.cur with
  | None -> []
  | Some c -> (
      match c with
      | c when is_whitespace c -> lex (advance lexer)
      | '0' .. '9' ->
          let token, new_lexer = lex_number lexer in
          token :: lex new_lexer
      | '+' -> Token.PLUS :: lex (advance lexer)
      | '-' -> Token.MINUS :: lex (advance lexer)
      | '*' -> Token.MULT :: lex (advance lexer)
      | '/' -> Token.DIV :: lex (advance lexer)
      | '(' -> Token.LPAREN :: lex (advance lexer)
      | ')' -> Token.RPAREN :: lex (advance lexer)
      | _ -> failwith "Unknown character")
