let read_whole_file path = In_channel.with_open_text path In_channel.input_all

let write_inst_to_file file inst =
  let open Pulse in
  let opcode = Bytecode.to_opcode inst in
  Out_channel.output_bytes file opcode;
  match inst with
  | Bytecode.PUSH num ->
      let bytes = Bytes.create 8 in
      Bytes.set_int64_le bytes 0 num;
      Out_channel.output_bytes file bytes
  | _ -> ()

let write_to_file path insts =
  Out_channel.with_open_bin path (fun file ->
      List.iter (write_inst_to_file file) insts)

let () =
  if Array.length Sys.argv <> 2 then exit 1
  else
    let open Pulse in
    read_whole_file Sys.argv.(1)
    |> Lexer.init |> Lexer.lex |> Parser.parse |> Compiler.compile
    |> write_to_file "a.out"
