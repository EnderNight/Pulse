let any = List.fold_left Bool.( || ) false
and all = List.fold_left Bool.( && ) true
and not_impl fun_name = failwith (fun_name ^ ": not implemented")
