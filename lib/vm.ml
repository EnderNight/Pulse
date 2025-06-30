let exec (bytecode : Bytecode.t) =
  let rec aux insts stack var_pool =
    match insts with
    | [] -> Error (Report.make "Missing HALT instruction")
    | Bytecode.HALT :: _ -> Ok (List.hd stack)
    | Bytecode.PUSH n :: tl -> aux tl (n :: stack) var_pool
    | Bytecode.ADD :: tl -> (
        match stack with
        | a :: b :: stl -> aux tl (Int64.add b a :: stl) var_pool
        | _ -> Error (Report.make "Not enough arguements"))
    | Bytecode.SUB :: tl -> (
        match stack with
        | a :: b :: stl -> aux tl (Int64.sub b a :: stl) var_pool
        | _ -> Error (Report.make "Not enough arguements"))
    | Bytecode.MULT :: tl -> (
        match stack with
        | a :: b :: stl -> aux tl (Int64.mul b a :: stl) var_pool
        | _ -> Error (Report.make "Not enough arguements"))
    | Bytecode.DIV :: tl -> (
        match stack with
        | a :: b :: stl ->
            if a = Int64.zero then
              Error (Report.make "Error: division by zero")
            else aux tl (Int64.div b a :: stl) var_pool
        | _ -> Error (Report.make "Not enough arguements"))
    | Bytecode.LOAD id :: tl -> aux tl (var_pool.(id) :: stack) var_pool
    | Bytecode.STORE id :: tl -> (
        match stack with
        | a :: stl ->
            Array.set var_pool id a;
            aux tl stl var_pool
        | _ -> Error (Report.make "Not enough arguments"))
  in
  aux bytecode.instructions []
    (Array.make bytecode.header.variable_pool_count Int64.zero)
