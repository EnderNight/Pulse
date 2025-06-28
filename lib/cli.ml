let exec input_file =
  Io.read_file input_file |> Lexer.make input_file |> Parser.parse
  |> Result.fold
       ~ok:(fun tree ->
         Compiler.compile tree |> Vm.exec |> Int64.to_string)
       ~error:Report.show
  |> print_endline

and compile input_file output_file =
  Io.read_file input_file |> Lexer.make input_file |> Parser.parse
  |> Result.fold
       ~ok:(fun tree ->
         Out_channel.with_open_bin output_file
           (Compiler.compile tree |> Bytecode.write_to_file))
       ~error:(fun report -> Report.show report |> print_endline)

and run input_file =
  In_channel.with_open_bin input_file Bytecode.read_from_file
  |> Vm.exec |> Int64.to_string |> print_endline

and disasm input_file =
  In_channel.with_open_bin input_file Bytecode.read_from_file
  |> List.iter (fun inst -> Bytecode.to_string inst |> print_endline)

open Cmdliner

let rec exec_cmd =
  let input_file = Arg.(required & pos 0 (some string) None & info []) in
  let exec_t = Term.(const exec $ input_file) in
  let info = Cmd.info "exec" in
  Cmd.v info exec_t

and compile_cmd =
  let input_file = Arg.(required & pos 0 (some string) None & info [])
  and output_file =
    Arg.(value & opt string "a.pulsebyc" & info [ "o" ])
  in
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
  let disasm_t = Term.(const disasm $ input_file) in
  let info = Cmd.info "disasm" in
  Cmd.v info disasm_t

and main () =
  let info = Cmd.info "pulse" in
  let cmd =
    Cmd.group info [ exec_cmd; compile_cmd; run_cmd; disasm_cmd ]
  in
  Cmd.eval cmd
