type t = { lexer : Lexer.t }

val make : Lexer.t -> t
val parse : t -> (Parsetree.t, Error.error) result
