type t = {
  source : string;
  line : int;
  column : int;
}

let make source = { source; line = 1; column = 1 }
and advance_col loc = { loc with column = loc.column + 1 }
and advance_line loc = { loc with line = loc.line + 1; column = 1 }

and report loc =
  loc.source ^ ":" ^ string_of_int loc.line ^ ":" ^ string_of_int loc.column
