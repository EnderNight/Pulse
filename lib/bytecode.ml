type header = {
  major : int;
  minor : int;
  patch : int;
  variable_pool_count : int;
}

type instruction =
  | HALT
  | PUSH of int64
  | ADD
  | SUB
  | MULT
  | MOD
  | DIV
  | LOAD of int
  | STORE of int
  | PRINT
  | JMP of int64
  | JNZ of int64
  | CEQ
  | CNE
  | CLT
  | CLE
  | CGT
  | CGE

type t = {
  header : header;
  instructions : instruction list;
}

let rec header_make variable_pool_count =
  let open Version in
  { major; minor; patch; variable_pool_count }

and make variable_pool_count instructions =
  let header = header_make variable_pool_count in
  { header; instructions }

and int_of_instruction inst =
  match inst with
  | HALT -> 0x0
  | PUSH _ -> 0x1
  | ADD -> 0x2
  | SUB -> 0x3
  | MULT -> 0x4
  | DIV -> 0x5
  | MOD -> 0x6
  | LOAD _ -> 0x7
  | STORE _ -> 0x8
  | PRINT -> 0x9
  | JMP _ -> 0xa
  | JNZ _ -> 0xb
  | CEQ -> 0xc
  | CNE -> 0xd
  | CLT -> 0xe
  | CLE -> 0xf
  | CGT -> 0x10
  | CGE -> 0x11

and bytes_of_instruction inst =
  let code = int_of_instruction inst
  and bytes =
    match inst with
    | PUSH n | JMP n | JNZ n ->
        let byte = Bytes.create 9 in
        Bytes.set_int64_be byte 1 n;
        byte
    | LOAD n | STORE n ->
        let byte = Bytes.create 3 in
        Bytes.set_uint16_be byte 1 n;
        byte
    | _ -> Bytes.create 1
  in
  Bytes.set_int8 bytes 0 code;
  bytes

and string_of_instruction inst =
  match inst with
  | HALT -> "HALT"
  | PUSH num -> "PUSH " ^ Int64.to_string num
  | ADD -> "ADD"
  | SUB -> "SUB"
  | MULT -> "MULT"
  | DIV -> "DIV"
  | MOD -> "MOD"
  | LOAD id -> "LOAD " ^ string_of_int id
  | STORE id -> "STORE " ^ string_of_int id
  | PRINT -> "PRINT"
  | JMP addr -> "JMP " ^ Int64.to_string addr
  | JNZ addr -> "JNZ " ^ Int64.to_string addr
  | CEQ -> "CEQ"
  | CNE -> "CNE"
  | CLT -> "CLT"
  | CLE -> "CLE"
  | CGT -> "CGT"
  | CGE -> "CGE"

and instruction_of_int code =
  match code with
  | 0x0 -> HALT
  | 0x1 -> PUSH Int64.zero
  | 0x2 -> ADD
  | 0x3 -> SUB
  | 0x4 -> MULT
  | 0x5 -> DIV
  | 0x6 -> MOD
  | 0x7 -> LOAD 0
  | 0x8 -> STORE 0
  | 0x9 -> PRINT
  | 0xa -> JMP Int64.zero
  | 0xb -> JNZ Int64.zero
  | 0xc -> CEQ
  | 0xd -> CNE
  | 0xe -> CLT
  | 0xf -> CLE
  | 0x10 -> CGT
  | 0x11 -> CGE
  | _ -> failwith "Unknown code"

and show_header header =
  let version =
    "v"
    ^ string_of_int header.major
    ^ "."
    ^ string_of_int header.minor
    ^ "."
    ^ string_of_int header.patch
  in
  "Pulse - compiled version: " ^ version ^ "\n" ^ "Variable pool count: "
  ^ string_of_int header.variable_pool_count
  ^ "\n"

and show_instructions insts =
  List.mapi
    (fun idx inst ->
      string_of_int idx ^ ":\t" ^ string_of_instruction inst)
    insts
  |> String.concat "\n"

and show bytecode =
  show_header bytecode.header
  ^ "\n"
  ^ show_instructions bytecode.instructions

and write_header_to_file header file =
  let bytes = Bytes.create 8 in
  Bytes.set_uint16_be bytes 0 header.major;
  Bytes.set_uint16_be bytes 2 header.minor;
  Bytes.set_uint16_be bytes 4 header.patch;
  Bytes.set_uint16_be bytes 6 header.variable_pool_count;
  Out_channel.output_bytes file bytes

and write_instructions_to_file insts file =
  let out_bytes inst =
    bytes_of_instruction inst |> Out_channel.output_bytes file
  in
  List.iter out_bytes insts

and write_to_file bytecode file =
  write_header_to_file bytecode.header file;
  write_instructions_to_file bytecode.instructions file

and read_header_from_file file =
  let bytes = Bytes.create 8 in
  match In_channel.really_input file bytes 0 8 with
  | None -> failwith "read_header_from_file: not enough bytes to read"
  | Some _ ->
      let get id = Bytes.get_uint16_be bytes id in
      let major = get 0
      and minor = get 2
      and patch = get 4
      and variable_pool_count = get 6 in
      { major; minor; patch; variable_pool_count }

and read_instructions_from_file file =
  let rec read_int n acc =
    if n = 0 then acc
    else
      match In_channel.input_byte file with
      | None -> failwith "read_int64: not enough bytes to read"
      | Some c ->
          read_int (n - 1) Int64.(logor (shift_left acc 8) (of_int c))
  in
  let read_int64 () = read_int 8 Int64.zero
  and read_uint16 () = read_int 2 Int64.zero in
  let rec aux acc =
    match In_channel.input_byte file with
    | Some code -> (
        let inst = instruction_of_int code in
        match inst with
        | PUSH _ ->
            let num = read_int64 () in
            aux (PUSH num :: acc)
        | LOAD _ ->
            let num = read_uint16 () in
            aux (LOAD (Int64.to_int num) :: acc)
        | STORE _ ->
            let num = read_uint16 () in
            aux (STORE (Int64.to_int num) :: acc)
        | JMP _ ->
            let addr = read_int64 () in
            aux (JMP addr :: acc)
        | JNZ _ ->
            let addr = read_int64 () in
            aux (JNZ addr :: acc)
        | _ -> aux (inst :: acc))
    | _ -> acc
  in
  List.rev (aux [])

and read_from_file file =
  let header = read_header_from_file file in
  let instructions = read_instructions_from_file file in
  { header; instructions }
