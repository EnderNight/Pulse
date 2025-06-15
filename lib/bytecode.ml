type t = PUSH of int64 | ADD | SUB | MULT | DIV | HALT [@@deriving show]

let rec to_int inst =
  match inst with
  | HALT -> 0x0
  | PUSH _ -> 0x1
  | ADD -> 0x2
  | SUB -> 0x3
  | MULT -> 0x4
  | DIV -> 0x5

and of_int code =
  match code with
  | 0x0 -> HALT
  | 0x1 -> PUSH (Int64.of_int 0)
  | 0x2 -> ADD
  | 0x3 -> SUB
  | 0x4 -> MULT
  | 0x5 -> DIV
  | _ -> failwith "Unknown opcode"

and write_to_file path insts =
  let write_int64 n file =
    let bytes = Bytes.create 8 in
    Bytes.set_int64_be bytes 0 n;
    Out_channel.output_bytes file bytes
  in
  let rec write_all insts file =
    match insts with
    | [] -> ()
    | inst :: insts -> (
        Out_channel.output_byte file (to_int inst);
        match inst with
        | PUSH n ->
            write_int64 n file;
            write_all insts file
        | _ -> write_all insts file)
  in
  Out_channel.with_open_bin path (write_all insts)

and read_from_file path =
  let read_int64 file =
    let rec aux shi acc =
      if shi = 0 then acc
      else
        match In_channel.input_byte file with
        | None -> acc
        | Some n ->
            aux (shi - 1) (Int64.add (Int64.shift_left acc 8) (Int64.of_int n))
    in
    aux 8 Int64.zero
  in
  let rec read_all acc file =
    match In_channel.input_byte file with
    | None -> List.rev acc
    | Some opcode -> (
        match of_int opcode with
        | PUSH _ ->
            let n = read_int64 file in
            read_all (PUSH n :: acc) file
        | inst -> read_all (inst :: acc) file)
  in
  In_channel.with_open_bin path (read_all [])
