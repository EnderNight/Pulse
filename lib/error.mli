type error = { loc : Location.t; msg : string }

val show_error : error -> string
