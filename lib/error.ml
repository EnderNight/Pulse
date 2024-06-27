type error = { loc : Location.t; msg : string }

let show_error error =
  error.loc.input_name ^ ":"
  ^ string_of_int error.loc.line
  ^ "."
  ^ string_of_int error.loc.col
  ^ ": " ^ error.msg
