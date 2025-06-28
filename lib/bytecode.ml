type t =
  | PUSH of int64
  | ADD
  | SUB
  | MULT
  | DIV
  | HALT

let rec to_code inst =
  match inst with
  | PUSH _ -> 0x1
  | ADD -> 0x2
  | SUB -> 0x3
  | MULT -> 0x4
  | DIV -> 0x5
  | HALT -> 0x0

and to_bytes inst =
  let code = to_code inst
  and bytes =
    match inst with
    | PUSH n ->
        let byte = Bytes.create 9 in
        Bytes.set_int64_be byte 1 n;
        byte
    | _ -> Bytes.create 1
  in
  Bytes.set_int8 bytes 0 code;
  bytes

and to_string inst =
  match inst with
  | PUSH num -> "PUSH " ^ Int64.to_string num
  | ADD -> "ADD"
  | SUB -> "SUB"
  | MULT -> "MULT"
  | DIV -> "DIV"
  | HALT -> "HALT"

and from_int code =
  match code with
  | 0x1 -> PUSH Int64.zero
  | 0x2 -> ADD
  | 0x3 -> SUB
  | 0x4 -> MULT
  | 0x5 -> DIV
  | 0x0 -> HALT
  | _ -> failwith "Unknown code"

and write_to_file insts file =
  match insts with
  | inst :: tl ->
      to_bytes inst |> Out_channel.output_bytes file;
      write_to_file tl file
  | _ -> ()

and read_from_file file =
  let rec read_int64 n acc =
    if n = 0 then acc
    else
      match In_channel.input_byte file with
      | Some c ->
          read_int64 (n - 1)
            (Int64.add (Int64.shift_left acc 8) (Int64.of_int c))
      | _ -> failwith "Not enough bytes to read"
  in
  let rec aux acc =
    match In_channel.input_byte file with
    | Some code -> (
        let inst = from_int code in
        match inst with
        | PUSH _ ->
            let num = read_int64 8 Int64.zero in
            aux (PUSH num :: acc)
        | _ -> aux (inst :: acc))
    | _ -> acc
  in
  List.rev (aux [])
