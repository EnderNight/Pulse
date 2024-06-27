(* Lexer *)
type t = {
  input : string;
  input_len : int;
  input_name : string;
  cur_char : char option;
  cursor : int;
  line : int;
  col : int;
}

val make : string -> string -> t
val lex : t -> (Token.t * t, Error.error) result
