let read_whole_file path = In_channel.with_open_text path In_channel.input_all

let () =
  if Array.length Sys.argv <> 2 then exit 1
  else
    let open Pulse in
    read_whole_file Sys.argv.(1)
    |> Lexer.init |> Lexer.lex |> List.map Token.show |> List.iter print_endline
