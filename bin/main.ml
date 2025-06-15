let () =
  let open Cmdliner in
  let info = Cmd.info "pulse" in
  let cmd = Cmd.group info Pulse.Cli.commands in
  exit (Cmd.eval cmd)
