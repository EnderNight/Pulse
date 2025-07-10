let read_file path = In_channel.with_open_text path In_channel.input_all

and any l =
  let rec aux l acc =
    match l with [] -> acc | e :: tl -> aux tl (e || acc)
  in
  aux l false

and not_impl fname = failwith (fname ^ ": not implemented")
