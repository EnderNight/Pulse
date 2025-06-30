let ( let* ) = Result.bind

let exec input_file =
  Result.fold
    ~ok:(fun result ->
      Int64.to_string result |> print_endline;
      0)
    ~error:(fun report ->
      Report.show report |> print_endline;
      1)
    (let* trees =
       Io.read_file input_file |> Lexer.make input_file |> Parser.parse
     in
     let* bound_trees, vpc = Binder.bind trees in
     Compiler.compile bound_trees vpc |> Vm.exec)

and compile input_file output_file =
  Result.fold
    ~ok:(fun _ -> 0)
    ~error:(fun report ->
      Report.show report |> print_endline;
      1)
    (let* trees =
       Io.read_file input_file |> Lexer.make input_file |> Parser.parse
     in
     let* bound_trees, vpc = Binder.bind trees in
     let bytecode = Compiler.compile bound_trees vpc in
     Ok
       (Out_channel.with_open_bin output_file
          (Bytecode.write_to_file bytecode)))

and run input_file =
  Result.fold
    ~ok:(fun result ->
      Int64.to_string result |> print_endline;
      0)
    ~error:(fun report ->
      Report.show report |> print_endline;
      1)
    (In_channel.with_open_bin input_file Bytecode.read_from_file
    |> Vm.exec)

and disasm input_file =
  In_channel.with_open_bin input_file Bytecode.read_from_file
  |> Bytecode.show |> print_endline;
  0

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
  let version =
    "v"
    ^ string_of_int Utils.major
    ^ "."
    ^ string_of_int Utils.minor
    ^ "."
    ^ string_of_int Utils.patch
  in
  let info = Cmd.info "pulse" ~version in
  let cmd =
    Cmd.group info [ exec_cmd; compile_cmd; run_cmd; disasm_cmd ]
  in
  Cmd.eval' cmd
