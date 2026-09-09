type typ =
  (* Constants *)
  | Integer of string
  | Ident of string
  (* Keywords *)
  | Print
  (* Operators *)
  | Plus
  | Minus
  | Mul
  | Div
  | Mod
  (* Ponctuators *)
  | SemiColon
  (* Misc *)
  | EOF

let to_keyword (s : string) = match s with "print" -> Some Print | _ -> None

and equal (t1 : typ) (t2 : typ) : bool =
  match (t1, t2) with
  | Integer x, Integer y -> x = y
  | Ident i, Ident j -> i = j
  | Print, Print -> true
  | Plus, Plus -> true
  | Minus, Minus -> true
  | Mul, Mul -> true
  | Div, Div -> true
  | Mod, Mod -> true
  | SemiColon, SemiColon -> true
  | EOF, EOF -> true
  | _ -> false

and string_of_typ t =
  match t with
  | Integer i -> i
  | Ident i -> i
  | Print -> "print"
  | Plus | Minus | Mul | Div | Mod -> "operator"
  | SemiColon -> ";"
  | EOF -> "end of file"
