open Cmdliner

let rec compile_cmd =
  let input_file = Arg.(required & pos 0 (some string) None & info [])
  and output_file = Arg.(value & opt string "a.out" & info [ "o" ]) in
  let compile_t =
    Term.(const Pulse.Driver.compile $ input_file $ output_file)
  in
  let info = Cmd.info "compile" in
  Cmd.v info compile_t

and main () =
  let version =
    "v"
    ^ string_of_int Pulse.Version.major
    ^ "."
    ^ string_of_int Pulse.Version.minor
    ^ "."
    ^ string_of_int Pulse.Version.patch
  in
  let info = Cmd.info "pulse" ~version in
  let cmd = Cmd.group info [ compile_cmd ] in
  Cmd.eval' cmd

let () = exit (main ())
