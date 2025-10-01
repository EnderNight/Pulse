let ( let* ) = Result.bind

let compile input_file output_file =
  let result =
    let* prog =
      Utils.read_file input_file |> Lexer.make input_file |> Parser.parse
    in
    let ir_str = Codegen.gen_program prog |> Qbe.show_program
    and out_ssa = output_file ^ ".ssa"
    and out_s = output_file ^ ".s" in
    let runtime_path =
      Option.value (Sys.getenv_opt "PULSE_RUNTIMEDIR") ~default:"/usr/lib"
    in
    let qbe_cmd = "qbe " ^ out_ssa ^ " -o " ^ out_s
    and cc_cmd =
      "gcc " ^ out_s ^ " -o " ^ output_file ^ " -L" ^ runtime_path
      ^ " -lpulsert"
    in
    Utils.write_file out_ssa ir_str;
    let qbe_out = Sys.command qbe_cmd in
    let cc_out = Sys.command cc_cmd in
    (* idc, it's funny *)
    Ok (qbe_out + cc_out)
  in
  match result with
  | Error report ->
      Report.show report |> prerr_endline;
      1
  | Ok r -> r

let main argc argv =
  if argc <> 3 then (
    prerr_endline "Invalid number of arguments";
    1)
  else compile argv.(1) argv.(2)
