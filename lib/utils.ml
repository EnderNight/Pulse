let any = List.fold_left Bool.( || ) false
and all = List.fold_left Bool.( && ) true
and not_impl fun_name = failwith (fun_name ^ ": not implemented")
and read_file path = In_channel.with_open_text path In_channel.input_all

and write_file path str =
  Out_channel.with_open_text path (fun oc -> Out_channel.output_string oc str)
