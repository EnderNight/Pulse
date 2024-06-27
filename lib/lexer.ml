open Error

type token_kind =
  (* Keywords *)
  | LET
  | FUN
  | RETURN
  | IF
  | ELSE
  | WHILE
  (* Identifier *)
  | ID of string
  (* Constants *)
  | INT_CONST of int
  (* String literal *)
  | STR_LIT of string
  (* Ponctuator *)
  | LPAREN
  | RPAREN
  | COLON
  | LBRACK
  | RBRACK
  | SEMICOLON
  | COMMA
  (* Operators *)
  | PLUS
  | MINUS
  | MULT
  | DIV
  | ASSIGN (* = *)
  | EQUAL (* == *)
  | NEQUAL (* != *)
  | LT (* < *)
  | LE (* <= *)
  | GT (* > *)
  | GE (* >= *)
  (* End of file *)
  | EOF

(* Token *)
type token = { kind : token_kind; loc : location }

(* Lexer *)
type lexer = {
  input : string;
  input_len : int;
  input_name : string;
  cur_char : char option;
  cursor : int;
  line : int;
  col : int;
}

let rec int_from_char c =
  match c with
  | '0' .. '9' -> int_of_char c - int_of_char '0'
  | _ -> failwith "int_from_char: c is not a digit"

and show_token_value token =
  match token.kind with
  | LET -> "let"
  | FUN -> "fun"
  | RETURN -> "return"
  | IF -> "if"
  | ELSE -> "else"
  | WHILE -> "while"
  | ID id -> id
  | INT_CONST i -> string_of_int i
  | STR_LIT s -> s
  | LPAREN -> "("
  | RPAREN -> ")"
  | COLON -> ":"
  | LBRACK -> "{"
  | RBRACK -> "}"
  | SEMICOLON -> ";"
  | COMMA -> ","
  | PLUS -> "+"
  | MINUS -> "-"
  | MULT -> "*"
  | DIV -> "/"
  | ASSIGN -> "="
  | EQUAL -> "=="
  | NEQUAL -> "!="
  | LT -> "<"
  | LE -> "<="
  | GT -> ">"
  | GE -> ">="
  | EOF -> "eof"

and is_digit = function '0' .. '9' -> true | _ -> false
and is_whitespace = function ' ' | '\n' | '\t' -> true | _ -> false

and is_id_start = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false

and is_id = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false

and make_loc lexer =
  {
    input_name = lexer.input_name;
    line = lexer.line;
    col = lexer.col;
  }

and make input name =
  let len = String.length input in
  {
    input;
    input_len = len;
    input_name = name;
    cur_char = (if len > 0 then Some input.[0] else None);
    cursor = 0;
    line = 1;
    col = 1;
  }

and get_token_kind token = token.kind

and get_kind_list tokens =
  match tokens with
  | [] -> []
  | token :: tokens -> token.kind :: get_kind_list tokens

and advance lexer =
  if lexer.cursor < lexer.input_len - 1 then
    let new_cursor = lexer.cursor + 1 in
    if lexer.input.[lexer.cursor] = '\n' then
      {
        lexer with
        cur_char = Some lexer.input.[new_cursor];
        cursor = new_cursor;
        line = lexer.line + 1;
        col = 1;
      }
    else
      {
        lexer with
        cur_char = Some lexer.input.[new_cursor];
        cursor = new_cursor;
        col = lexer.col + 1;
      }
  else { lexer with cur_char = None }

and advance_n lexer n =
  if lexer.cursor + n >= lexer.input_len then
    { lexer with cur_char = None }
  else
    let rec advance_n_rec lexer n =
      match n with
      | n when n < 1 -> lexer
      | 1 -> advance lexer
      | _ -> advance_n_rec (advance lexer) (n - 1)
    in
    advance_n_rec lexer n

and try_lex lexer expected =
  let len = String.length expected in
  let try_lex_char lexer exp_char =
    match lexer.cur_char with
    | Some c when c = exp_char -> true
    | _ -> false
  in
  let rec try_rec lexer ind =
    match ind with
    | i when i >= len -> true
    | i when try_lex_char lexer expected.[i] ->
        try_rec (advance lexer) (ind + 1)
    | _ -> false
  in
  try_rec lexer 0

and lex_int lexer loc =
  let rec lex_int_acc lexer acc =
    match lexer.cur_char with
    | Some c when is_digit c ->
        lex_int_acc (advance lexer) ((acc * 10) + int_from_char c)
    | _ -> Ok ({ kind = INT_CONST acc; loc }, lexer)
  in
  match lexer.cur_char with
  | Some '0' .. '9' -> lex_int_acc lexer 0
  (* Debug *)
  | None -> Error { loc; msg = "unexpected end of file" }
  | Some _ -> Error { loc; msg = "expecting a digit" }

and lex_string lexer loc =
  let rec lex_string_acc lexer acc =
    match lexer.cur_char with
    | None -> Error { loc; msg = "unexpected end of file" }
    | Some '"' -> Ok ({ kind = STR_LIT acc; loc }, advance lexer)
    | Some c -> lex_string_acc (advance lexer) (acc ^ String.make 1 c)
  in
  match lexer.cur_char with
  | Some '"' -> lex_string_acc (advance lexer) ""
  | Some _ -> Error { loc; msg = "expecting a \"" }
  (* Debug *)
  | None -> Error { loc; msg = "unexpected end of file" }

and lex_id lexer loc =
  let rec lex_id_acc lexer acc =
    match lexer.cur_char with
    | Some c when is_id c ->
        lex_id_acc (advance lexer) (acc ^ String.make 1 c)
    | _ -> Ok ({ kind = ID acc; loc }, lexer)
  in
  match lexer.cur_char with
  | Some c when is_id_start c -> lex_id_acc lexer ""
  (* Debug *)
  | None -> Error { loc; msg = "unexpected end of file" }
  | _ -> Error { loc; msg = "expecting an identifier" }

and lex lexer =
  let loc = make_loc lexer in
  match lexer.cur_char with
  (* End of file *)
  | None -> Ok ({ kind = EOF; loc }, lexer)
  (* Whitespaces *)
  | Some c when is_whitespace c -> lex (advance lexer)
  (* Constants *)
  | Some c when is_digit c -> lex_int lexer loc
  (* Literals *)
  | Some '"' -> lex_string lexer loc
  (* Ponctuators *)
  | Some '(' -> Ok ({ kind = LPAREN; loc }, advance lexer)
  | Some ')' -> Ok ({ kind = RPAREN; loc }, advance lexer)
  | Some ':' -> Ok ({ kind = COLON; loc }, advance lexer)
  | Some '{' -> Ok ({ kind = LBRACK; loc }, advance lexer)
  | Some '}' -> Ok ({ kind = RBRACK; loc }, advance lexer)
  | Some ';' -> Ok ({ kind = SEMICOLON; loc }, advance lexer)
  | Some ',' -> Ok ({ kind = COMMA; loc }, advance lexer)
  (* Operators *)
  | Some '+' -> Ok ({ kind = PLUS; loc }, advance lexer)
  | Some '-' -> Ok ({ kind = MINUS; loc }, advance lexer)
  | Some '*' -> Ok ({ kind = MULT; loc }, advance lexer)
  | Some '/' -> Ok ({ kind = DIV; loc }, advance lexer)
  | Some '=' ->
      if try_lex lexer "==" then
        Ok ({ kind = EQUAL; loc }, advance_n lexer 2)
      else Ok ({ kind = ASSIGN; loc }, advance lexer)
  | Some '<' ->
      if try_lex lexer "<=" then
        Ok ({ kind = LE; loc }, advance_n lexer 2)
      else Ok ({ kind = LT; loc }, advance lexer)
  | Some '>' ->
      if try_lex lexer ">=" then
        Ok ({ kind = GE; loc }, advance_n lexer 2)
      else Ok ({ kind = GT; loc }, advance lexer)
  | Some '!' when try_lex lexer "!=" ->
      Ok ({ kind = NEQUAL; loc }, advance_n lexer 2)
  (* Keywords *)
  | Some 'l' when try_lex lexer "let" ->
      Ok ({ kind = LET; loc }, advance_n lexer 3)
  | Some 'f' when try_lex lexer "fun" ->
      Ok ({ kind = FUN; loc }, advance_n lexer 3)
  | Some 'r' when try_lex lexer "return" ->
      Ok ({ kind = RETURN; loc }, advance_n lexer 6)
  | Some 'i' when try_lex lexer "if" ->
      Ok ({ kind = IF; loc }, advance_n lexer 2)
  | Some 'e' when try_lex lexer "else" ->
      Ok ({ kind = ELSE; loc }, advance_n lexer 4)
  | Some 'w' when try_lex lexer "while" ->
      Ok ({ kind = WHILE; loc }, advance_n lexer 5)
  (* Identifiers *)
  | Some c when is_id_start c -> lex_id lexer loc
  (* Unknown char *)
  | Some c ->
      Error { loc; msg = "unexpected char '" ^ String.make 1 c ^ "'" }
