let ( let* ) = Result.bind

let compile dump_parsetree input_file output_file =
  let result =
    let* prog =
      Utils.read_file input_file |> Lexer.make input_file |> Parser.parse
    in
    if dump_parsetree then (
      Parsetree.to_dot prog |> print_endline;
      Ok 0)
    else
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

let print_usage () = prerr_endline "USAGE: pulse [FLAGS] INPUT_FILE OUTPUT_FILE"

let parse_flag fname argv idx =
  match argv.(idx) with
  | f when String.equal fname f -> (true, idx + 1)
  | _ -> (false, idx)

let main argc argv =
  if argc < 3 then (
    prerr_endline "Invalid number of arguments";
    print_usage ();
    1)
  else
    let dump_parsetree, idx = parse_flag "--dump-parsetree" argv 1 in
    compile dump_parsetree argv.(idx) argv.(idx + 1)
