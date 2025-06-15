open Cmdliner

let compile input_file output_file =
  Io.read_whole_file input_file
  |> Lexer.init |> Lexer.lex |> Parser.parse |> Compiler.compile
  |> Bytecode.write_to_file output_file

let run input_file =
  Bytecode.read_from_file input_file
  |> Vm.run |> Int64.to_string |> print_endline

let disasm input_file =
  Bytecode.read_from_file input_file
  |> List.iter (fun inst -> print_endline (Bytecode.show inst))

let rec compile_cmd =
  let input_file = Arg.(required & pos 0 (some string) None & info [])
  and output_file = Arg.(value & opt string "a.pulsebyc" & info [ "o" ]) in
  let compile_t = Term.(const compile $ input_file $ output_file) in
  let info = Cmd.info "compile" in
  Cmd.v info compile_t

and run_cmd =
  let input_file = Arg.(required & pos 0 (some string) None & info []) in
  let run_t = Term.(const run $ input_file) in
  let info = Cmd.info "run" in
  Cmd.v info run_t

and disasm_cmd =
  let input_file = Arg.(required & pos 0 (some string) None & info []) in
  let disassemble_t = Term.(const disasm $ input_file) in
  let info = Cmd.info "disasm" in
  Cmd.v info disassemble_t

and commands = [ compile_cmd; run_cmd; disasm_cmd ]
