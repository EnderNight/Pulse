type t = {
  loc : Location.t option;
  msg : string;
}

let make_loc loc msg = { loc = Some loc; msg }
and make msg = { loc = None; msg }

and show report =
  Option.fold ~none:"" ~some:(fun loc -> Location.report loc ^ ": ") report.loc
  ^ report.msg
