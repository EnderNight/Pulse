let compile _ _ = 1

open Cmdliner

let rec compile_cmd =
  let input_file = Arg.(required & pos 0 (some string) None & info [])
  and output_file =
    Arg.(value & opt string "a.pulsebyc" & info [ "o" ])
  in
  let compile_t = Term.(const compile $ input_file $ output_file) in
  let info = Cmd.info "compile" in
  Cmd.v info compile_t

and main () =
  let version =
    "v"
    ^ string_of_int Version.major
    ^ "."
    ^ string_of_int Version.minor
    ^ "."
    ^ string_of_int Version.patch
  in
  let info = Cmd.info "pulse" ~version in
  let cmd = Cmd.group info [ compile_cmd ] in
  Cmd.eval' cmd
