type t = {
  loc : Location.t;
  msg : string;
}

let make loc msg = { loc; msg }
let show report = Location.report report.loc ^ ": " ^ report.msg
