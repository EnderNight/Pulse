let exec (bytecode : Bytecode.t) =
  let rec aux insts idx stack var_pool =
    let next_idx = idx + 1 in
    match List.nth insts idx with
    | Bytecode.HALT -> Ok ()
    | Bytecode.PUSH n -> aux insts next_idx (n :: stack) var_pool
    | Bytecode.ADD -> (
        match stack with
        | a :: b :: stl ->
            aux insts next_idx (Int64.add b a :: stl) var_pool
        | _ -> Error (Report.make "Not enough arguements"))
    | Bytecode.SUB -> (
        match stack with
        | a :: b :: stl ->
            aux insts next_idx (Int64.sub b a :: stl) var_pool
        | _ -> Error (Report.make "Not enough arguements"))
    | Bytecode.MULT -> (
        match stack with
        | a :: b :: stl ->
            aux insts next_idx (Int64.mul b a :: stl) var_pool
        | _ -> Error (Report.make "Not enough arguements"))
    | Bytecode.DIV -> (
        match stack with
        | a :: b :: stl ->
            if a = Int64.zero then
              Error (Report.make "Error: division by zero")
            else aux insts next_idx (Int64.div b a :: stl) var_pool
        | _ -> Error (Report.make "Not enough arguements"))
    | Bytecode.MOD -> (
        match stack with
        | a :: b :: stl ->
            if a = Int64.zero then
              Error (Report.make "Error: division by zero")
            else aux insts next_idx (Int64.rem b a :: stl) var_pool
        | _ -> Error (Report.make "Not enough arguements"))
    | Bytecode.LOAD id ->
        aux insts next_idx (var_pool.(id) :: stack) var_pool
    | Bytecode.STORE id -> (
        match stack with
        | a :: stl ->
            Array.set var_pool id a;
            aux insts next_idx stl var_pool
        | _ -> Error (Report.make "Not enough arguments"))
    | Bytecode.PRINT -> (
        match stack with
        | a :: stl ->
            Int64.to_int a |> Char.chr |> print_char;
            aux insts next_idx stl var_pool
        | _ -> Error (Report.make "Not engough arguements"))
    | Bytecode.PRINT_INT -> (
        match stack with
        | a :: stl ->
            Int64.to_string a |> print_string;
            aux insts next_idx stl var_pool
        | _ -> Error (Report.make "Not engough arguements"))
    | Bytecode.JMP addr -> aux insts (Int64.to_int addr) stack var_pool
    | Bytecode.JNZ addr -> (
        match stack with
        | a :: stl ->
            if a = Int64.zero then aux insts next_idx stl var_pool
            else aux insts (Int64.to_int addr) stl var_pool
        | _ -> Error (Report.make "Not engough arguements"))
    | Bytecode.CEQ -> (
        match stack with
        | a :: b :: stl ->
            if b = a then aux insts next_idx (Int64.one :: stl) var_pool
            else aux insts next_idx (Int64.zero :: stl) var_pool
        | _ -> Error (Report.make "Not engough arguements"))
    | Bytecode.CNE -> (
        match stack with
        | a :: b :: stl ->
            if b <> a then aux insts next_idx (Int64.one :: stl) var_pool
            else aux insts next_idx (Int64.zero :: stl) var_pool
        | _ -> Error (Report.make "Not engough arguements"))
    | Bytecode.CLT -> (
        match stack with
        | a :: b :: stl ->
            if b < a then aux insts next_idx (Int64.one :: stl) var_pool
            else aux insts next_idx (Int64.zero :: stl) var_pool
        | _ -> Error (Report.make "Not engough arguements"))
    | Bytecode.CLE -> (
        match stack with
        | a :: b :: stl ->
            if b <= a then aux insts next_idx (Int64.one :: stl) var_pool
            else aux insts next_idx (Int64.zero :: stl) var_pool
        | _ -> Error (Report.make "Not engough arguements"))
    | Bytecode.CGT -> (
        match stack with
        | a :: b :: stl ->
            if b > a then aux insts next_idx (Int64.one :: stl) var_pool
            else aux insts next_idx (Int64.zero :: stl) var_pool
        | _ -> Error (Report.make "Not engough arguements"))
    | Bytecode.CGE -> (
        match stack with
        | a :: b :: stl ->
            if b >= a then aux insts next_idx (Int64.one :: stl) var_pool
            else aux insts next_idx (Int64.zero :: stl) var_pool
        | _ -> Error (Report.make "Not engough arguements"))
  in
  aux bytecode.instructions 0 []
    (Array.make bytecode.header.variable_pool_count Int64.zero)
