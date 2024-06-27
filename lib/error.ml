(* Token location *)
type location = { input_name : string; line : int; col : int }

(* General error *)
type error = { loc : location; msg : string }

let show_error error =
  error.loc.input_name ^ ":"
  ^ string_of_int error.loc.line
  ^ "."
  ^ string_of_int error.loc.col
  ^ ": " ^ error.msg
