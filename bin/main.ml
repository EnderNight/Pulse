let () =
  let input =
    In_channel.with_open_text (Array.get Sys.argv 1) In_channel.input_all
  in
  let lexer = Pulse.Lexer.make input in
  let ret_code =
    Result.fold
      ~ok:(fun (tree, _) ->
        let ir = Pulse.Ir.flatten tree in
        let program = Pulse.Codegen.codegen ir in
        Out_channel.with_open_text "main.pulse.asm" (fun c ->
            Out_channel.output_string c program);
        Sys.command
          "nasm -felf64 main.pulse.asm && ld main.pulse.o -o main.pulse.exe")
      ~error:(fun e ->
        print_endline e;
        1)
      (Pulse.Parser.parse_program lexer)
  in
  exit ret_code
