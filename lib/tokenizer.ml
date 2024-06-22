type token =
  (* Constants *)
  | IDENTIFIER of string
  | INT_LITERAL of int
  | STR_LITERAL of string
  (* Reserved words *)
  | LET
  | IF
  | ELSE
  | RETURN
  | WHILE
  (* Structure *)
  | COLON
  | SEMICOLON
  | LPAREN
  | RPAREN
  | LBRACK
  | RBRACK
  | COMA
  (* Operators *)
  | PLUS
  | MINUS
  | MULT
  | DIV
  | LT
  | GT
  | LE
  | GE
  | ASSIGN
  | EQUAL
  | DIFF
  (* Misc *)
  | EOF
[@@deriving show]

let string_of_token = function
  (* Constants *)
  | IDENTIFIER id -> id
  | INT_LITERAL i -> string_of_int i
  | STR_LITERAL s -> "\"" ^ s ^ "\""
  (* Reserved words *)
  | LET -> "let"
  | IF -> "if"
  | ELSE -> "else"
  | RETURN -> "return"
  | WHILE -> "while"
  (* Structure *)
  | COLON -> ":"
  | SEMICOLON -> ";"
  | LPAREN -> "("
  | RPAREN -> ")"
  | LBRACK -> "{"
  | RBRACK -> "}"
  | COMA -> ","
  (* Operators *)
  | PLUS -> "+"
  | MINUS -> "-"
  | MULT -> "*"
  | DIV -> "/"
  | LT -> "<"
  | GT -> ">"
  | LE -> "<="
  | GE -> ">="
  | ASSIGN -> "="
  | EQUAL -> "=="
  | DIFF -> "!="
  (* Misc *)
  | EOF -> "EOF"

let rec get_digit c =
  match c with
  | '0' .. '9' -> Some (int_of_char c - int_of_char '0')
  | _ -> None

and lex_string input pos length =
  let rec get_string pos =
    if pos >= length then ("", pos)
    else
      match input.[pos] with
      | '"' -> ("", pos + 1)
      | c ->
          let sub, pos = get_string (pos + 1) in
          (String.make 1 c ^ sub, pos)
  in
  get_string pos

and lex_int input pos length =
  let rec get_int pos num =
    if pos >= length then (0, pos)
    else
      match get_digit input.[pos] with
      | Some digit -> get_int (pos + 1) ((num * 10) + digit)
      | None -> (num, pos)
  in
  get_int pos 0

and lex_identifier input pos length =
  let rec get_id pos =
    if pos >= length then ("", pos)
    else
      match input.[pos] with
      | 'a' .. 'z' | 'A' .. 'Z' ->
          let sub, new_pos = get_id (pos + 1) in
          (String.make 1 input.[pos] ^ sub, new_pos)
      | _ -> ("", pos)
  in
  get_id pos

and is_expected input pos expected =
  let length = String.length input and len = String.length expected in
  if pos + len >= length then false
  else String.sub input pos len = expected

and lex input =
  let length = String.length input in
  let rec next_token pos =
    if pos >= length then [ EOF ]
    else
      match input.[pos] with
      | ' ' | '\t' | '\n' -> next_token (pos + 1)
      | 'l' when is_expected input pos "let" ->
          LET :: next_token (pos + 3)
      | ':' -> COLON :: next_token (pos + 1)
      | ';' -> SEMICOLON :: next_token (pos + 1)
      | '(' -> LPAREN :: next_token (pos + 1)
      | ')' -> RPAREN :: next_token (pos + 1)
      | '{' -> LBRACK :: next_token (pos + 1)
      | '}' -> RBRACK :: next_token (pos + 1)
      | ',' -> COMA :: next_token (pos + 1)
      | '=' ->
          if is_expected input pos "==" then
            EQUAL :: next_token (pos + 2)
          else ASSIGN :: next_token (pos + 1)
      | '!' when is_expected input pos "!=" ->
          DIFF :: next_token (pos + 2)
      | 'i' when is_expected input pos "if" ->
          IF :: next_token (pos + 2)
      | 'e' when is_expected input pos "else" ->
          ELSE :: next_token (pos + 4)
      | 'r' when is_expected input pos "return" ->
          RETURN :: next_token (pos + 6)
      | 'w' when is_expected input pos "while" ->
          WHILE :: next_token (pos + 5)
      | '+' -> PLUS :: next_token (pos + 1)
      | '-' -> MINUS :: next_token (pos + 1)
      | '*' -> MULT :: next_token (pos + 1)
      | '/' -> DIV :: next_token (pos + 1)
      | '<' ->
          if is_expected input pos "<=" then LE :: next_token (pos + 2)
          else LT :: next_token (pos + 1)
      | '>' ->
          if is_expected input pos ">=" then GE :: next_token (pos + 2)
          else GT :: next_token (pos + 1)
      | '0' .. '9' ->
          let num, pos = lex_int input pos length in
          INT_LITERAL num :: next_token pos
      | '"' ->
          let str, pos = lex_string input (pos + 1) length in
          STR_LITERAL str :: next_token pos
      | 'a' .. 'z' | 'A' .. 'Z' ->
          let id, pos = lex_identifier input pos length in
          IDENTIFIER id :: next_token pos
      | c -> failwith ("unexpected char " ^ String.make 1 c)
  in
  next_token 0
