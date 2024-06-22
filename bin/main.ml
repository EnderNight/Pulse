let () =
  let open Pulse in
  for i = 1 to Array.length Sys.argv - 1 do
    let file_content =
      print_endline Sys.argv.(i);
      In_channel.with_open_text Sys.argv.(i) In_channel.input_all
    in
    let tokens = Tokenizer.lex file_content in
    let parseast = Parser.parse tokens in
    Parser.show_ast parseast |> print_endline
  done
