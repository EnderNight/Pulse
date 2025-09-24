type token_kind =
  (* Constants *)
  | INT of string
  | IDENT of string
  (* Operators *)
  | PLUS
  | MINUS
  | MULT
  | DIV
  | MOD
  | EQ
  | DEQ
  | NEQ
  | LT
  | LE
  | GT
  | GE
  (* Punctuators *)
  | LPAREN
  | RPAREN
  | LBRACK
  | RBRACK
  | SEMICOLON
  (* Keywords *)
  | LET
  | PRINT
  | PRINT_INT
  | IF
  | ELSE
  | WHILE
  (* Misc *)
  | EOF

type t = {
  kind : token_kind;
  loc : Location.t;
}

let make kind loc = { kind; loc }

and name_of_token_kind = function
  | INT _ -> "number"
  | IDENT _ -> "identifier"
  | PLUS -> "plus sign"
  | MINUS -> "minus sign"
  | MULT -> "multiplication sign"
  | DIV -> "division sign"
  | MOD -> "reminder sign"
  | EQ -> "equal sign"
  | DEQ -> "double equal sign"
  | NEQ -> "not equal sign"
  | LT -> "less than sign"
  | LE -> "less than or equal sign"
  | GT -> "greater than sign"
  | GE -> "greater than or equal sign"
  | LPAREN -> "left parenthesis"
  | RPAREN -> "right parenthesis"
  | LBRACK -> "left bracket"
  | RBRACK -> "right bracket"
  | SEMICOLON -> "semicolon"
  | LET -> "'let'"
  | PRINT -> "'print'"
  | PRINT_INT -> "'print_int'"
  | IF -> "'if'"
  | ELSE -> "'else'"
  | WHILE -> "'while'"
  | EOF -> "end of file"

and keyword_of_string_opt = function
  | "let" -> Some LET
  | "print" -> Some PRINT
  | "print_int" -> Some PRINT_INT
  | "if" -> Some IF
  | "else" -> Some ELSE
  | "while" -> Some WHILE
  | _ -> None

and token_kind_loose_equal t1 t2 =
  match (t1, t2) with
  | INT _, INT _ -> true
  | IDENT _, IDENT _ -> true
  | _ -> t1 = t2
