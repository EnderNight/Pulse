type token = 
    | LET
    | COLON
    | SEMICOLON
    | LPAREN
    | RPAREN
    | LBRACK
    | RBRACK
    | COMA
    | EQUAL
    | IF
    | ELSE
    | RETURN
    | WHILE
    
    (* Operators *)
    | PLUS
    | MINUS
    | MULT
    | DIV
    | LT
    | GT

    | EOF
    | IDENTIFIER of string
    | INT_LITERAL of int
    | STR_LITERAL of string

let string_of_token = function
    | LET -> "LET"
    | COLON -> "COLON"
    | SEMICOLON -> "SEMICOLON"
    | LPAREN -> "LPAREN"
    | RPAREN -> "RPAREN"
    | LBRACK -> "LBRACK"
    | RBRACK -> "RBRACK"
    | COMA -> "COMA"
    | EQUAL -> "EQUAL"
    | IF -> "IF"
    | ELSE -> "ELSE"
    | RETURN -> "RETURN"
    | WHILE -> "WHILE"
    | PLUS -> "PLUS"
    | MINUS -> "MINUS"
    | MULT -> "MULT"
    | DIV -> "DIV"
    | LT -> "LT"
    | GT -> "GT"
    | EOF -> "EOF"
    | IDENTIFIER id -> "IDENTIFIER(" ^ id ^ ")"
    | INT_LITERAL i -> "INT_LITERAL(" ^ string_of_int i ^ ")"
    | STR_LITERAL s -> "STR_LITERAL(" ^ s ^ ")"
;;

let get_digit c =
    match c with
    | '0'..'9' -> Some (int_of_char c - int_of_char '0')
    | _ -> None
;;

let lex_string input pos length =
    let rec next_string pos = 
        if pos >= length then "", pos
        else
            match input.[pos] with
            | '"' -> ("", pos + 1)
            | c -> let sub, pos = next_string (pos + 1)
                        in ((String.make 1 c) ^ sub, pos)
    in next_string pos
;;

let lex_int input pos length =
    let rec next_int pos num =
        if pos >= length then 0, pos
        else 
            match get_digit input.[pos] with
            | Some digit -> next_int (pos + 1) (num * 10 + digit)
            | None -> num, pos
    in next_int pos 0
;;

let lex_identifier input pos length =
    let rec next_id pos =
        if pos >= length then "", pos
        else
            match input.[pos] with
            | 'a'..'z' | 'A'..'Z' -> let sub, new_pos = next_id (pos + 1)
                                        in ((String.make 1 input.[pos]) ^ sub, new_pos)
            | _ -> "", pos
    in next_id pos
;;

let lex input =
    let length = String.length input in
    let rec next_token pos =
        if pos >= length then [EOF]
        else
            match input.[pos] with
            | ' ' | '\t' | '\n' -> next_token (pos + 1)
            | 'l' when String.sub input pos 3 = "let" -> LET :: next_token (pos + 3)
            | ':' -> COLON :: next_token (pos + 1)
            | ';' -> SEMICOLON :: next_token (pos + 1)
            | '(' -> LPAREN :: next_token (pos + 1)
            | ')' -> RPAREN :: next_token (pos + 1)
            | '{' -> LBRACK :: next_token (pos + 1)
            | '}' -> RBRACK :: next_token (pos + 1)
            | ',' -> COMA :: next_token (pos + 1)
            | '=' -> EQUAL :: next_token (pos + 1)
            | 'i' when String.sub input pos 2 = "if" -> IF :: next_token (pos + 2)
            | 'e' when String.sub input pos 4 = "else" -> ELSE :: next_token (pos + 4)
            | 'r' when String.sub input pos 6 = "return" -> RETURN :: next_token (pos + 6)
            | 'w' when String.sub input pos 5 = "while" -> WHILE :: next_token (pos + 5)
            | '+' -> PLUS :: next_token (pos + 1)
            | '-' -> MINUS :: next_token (pos + 1)
            | '*' -> MULT :: next_token (pos + 1)
            | '/' -> DIV :: next_token (pos + 1)
            | '<' -> LT :: next_token (pos + 1)
            | '>' -> GT :: next_token (pos + 1)
            | '0'..'9' -> let num, pos = lex_int input pos length in INT_LITERAL num :: next_token pos
            | '"' -> let str, pos = lex_string input (pos + 1) length in STR_LITERAL str :: next_token pos
            | 'a'..'z' | 'A'..'Z' -> let id, pos = lex_identifier input pos length in IDENTIFIER id :: next_token pos
            | _ -> failwith "unexpected char"
    in next_token 0
;;
