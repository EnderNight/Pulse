type t = { acc : int }

let make = { acc = 0 }

and create symbol s =
  (s ^ string_of_int symbol.acc, { acc = symbol.acc + 1 })
