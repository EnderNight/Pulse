type t = { input_name : string; line : int; col : int }

val make : string -> int -> int -> t
