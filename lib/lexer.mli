type t

val make : string -> string -> t
val next_token : t -> (Token.t * t, Report.t) result
