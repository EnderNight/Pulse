let ( let* ) = Result.bind

let compile input_file output_file =
  let result =
    let ir_output = output_file ^ ".pulsir" in
    let* trees =
      Utils.read_file input_file |> Lexer.make input_file |> Parser.parse
    in
    let ir = Codegen_ir.gen_ir trees in
    Out_channel.with_open_text ir_output (fun file ->
        Out_channel.output_string file (Ir.show_ir_program ir));
    Ok 0
  in
  Result.fold ~ok:Fun.id
    ~error:(fun report ->
      Report.show report |> print_endline;
      1)
    result

(* let compile input_file output_file = *)
(*   let asm_output = output_file ^ ".s" *)
(*   and obj_output = output_file ^ ".o" in *)
(*   let result = *)
(*     let* trees = *)
(*       Utils.read_file input_file |> Lexer.make input_file |> Parser.parse *)
(*     in *)
(*     let asm = Codegen.gen_ir trees |> Codegen.gen_x86_64 in *)
(*     Out_channel.with_open_text asm_output (fun file -> *)
(*         Out_channel.output_string file asm); *)
(*     let code = *)
(*       Sys.command *)
(*         ("as " ^ asm_output ^ " -o " ^ obj_output ^ " && ld " ^ obj_output *)
(*        ^ " -o " ^ output_file) *)
(*     in *)
(*     Ok code *)
(*   in *)
(*   Result.fold ~ok:Fun.id *)
(*     ~error:(fun report -> *)
(*       Report.show report |> print_endline; *)
(*       1) *)
(*     result *)
