type flags = { dump_parsetree : bool }

type pipeline_stage =
  (* Frontend stages *)
  | IoIn
  | Parse of Lexer.t
  | Codegen of Parsetree.program
  | IoOut of Qbe.program
  (* QBE stages *)
  | QbeCompiler
  (* Assembler stages *)
  | Assembler

type pipeline = {
  infile : string;
  outfile : string;
  flags : flags;
  stage : pipeline_stage;
}

let ( let* ) = Result.bind

let pipeline_run_stage pipeline =
  match pipeline.stage with
  | IoIn ->
      let lexer =
        Utils.read_file pipeline.infile |> Lexer.make pipeline.infile
      in
      Ok (Some (Parse lexer))
  | Parse lexer -> (
      match Parser.parse lexer with
      | Ok program ->
          if pipeline.flags.dump_parsetree then (
            Parsetree.to_dot program |> print_endline;
            Ok None)
          else Ok (Some (Codegen program))
      | Error report ->
          Report.show report |> prerr_endline;
          Error 1)
  | Codegen program ->
      let program = Codegen.gen_program program in
      Ok (Some (IoOut program))
  | IoOut program ->
      let outfile_ssa = pipeline.outfile ^ ".ssa"
      and program_str = Qbe.show_program program in
      Utils.write_file outfile_ssa program_str;
      Ok (Some QbeCompiler)
  | QbeCompiler ->
      let outfile_ssa = pipeline.outfile ^ ".ssa"
      and outfile_s = pipeline.outfile ^ ".s" in
      let qbe_cmd = "qbe " ^ outfile_ssa ^ " -o " ^ outfile_s in
      let ret = Sys.command qbe_cmd in
      if ret = 0 then Ok (Some Assembler) else Error ret
  | Assembler ->
      let outfile_s = pipeline.outfile ^ ".s" in
      let as_cmd = "gcc " ^ outfile_s ^ " -o " ^ pipeline.outfile in
      let as_cmd =
        match Sys.getenv_opt "PULSE_RUNTIMEDIR" with
        | None -> as_cmd
        | Some var -> as_cmd ^ " -L" ^ var ^ " -lpulsert"
      in
      let ret = Sys.command as_cmd in
      if ret = 0 then Ok None else Error ret

let pipeline_run pipeline =
  let rec aux pipeline =
    match pipeline_run_stage pipeline with
    | Ok (Some stage) ->
        let pipeline = { pipeline with stage } in
        aux pipeline
    | Ok None -> 0
    | Error ret -> ret
  in
  aux pipeline

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
    let flags = { dump_parsetree } in
    let pipeline =
      { infile = argv.(idx); outfile = argv.(idx + 1); flags; stage = IoIn }
    in
    pipeline_run pipeline
