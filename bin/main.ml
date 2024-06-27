open Pulse

(* let rec lex_all lexer = *)
(*   match Lexer.lex lexer with *)
(*   | Error error -> Error error *)
(*   | Ok (token, _) when token.kind = EOF -> Ok [ token ] *)
(*   | Ok (token, lexer) -> ( *)
(*       match lex_all lexer with *)
(*       | Error error -> Error error *)
(*       | Ok tokens -> Ok (token :: tokens)) *)

let () =
  if Array.length Sys.argv != 2 then
    failwith "Incorrect number of arguements."
  else
    let input =
      In_channel.with_open_text Sys.argv.(1) In_channel.input_all
    in
    let lexer = Lexer.make input Sys.argv.(1) in
    let parser = Parser.make lexer in
    match Parser.parse parser with
    | Ok (program, _) -> Parser.show_program program |> print_endline
    | Error error ->
        Error.show_error error |> print_endline;
        exit 1
