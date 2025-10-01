let () =
  let argc = Array.length Sys.argv in
  let ret = Pulse.Cli.main argc Sys.argv in
  exit ret
