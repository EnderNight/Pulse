let major = 0
and minor = 2
and patch = 0

and any l =
  let rec aux l acc =
    match l with [] -> acc | e :: tl -> aux tl (e || acc)
  in
  aux l false
