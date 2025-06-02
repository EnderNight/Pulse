type t = PUSH of int64 | ADD | SUB | MULT | DIV | HALT [@@deriving show]

let to_opcode inst =
  let opcode =
    match inst with
    | HALT -> 0x0
    | PUSH _ -> 0x1
    | ADD -> 0x2
    | SUB -> 0x3
    | MULT -> 0x4
    | DIV -> 0x5
  in
  let bytes = Bytes.create 1 in
  Bytes.set_int8 bytes 0 opcode;
  bytes
