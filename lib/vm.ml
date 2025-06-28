let exec insts =
  let rec aux insts stack =
    match insts with
    | Bytecode.PUSH n :: tl -> aux tl (n :: stack)
    | Bytecode.ADD :: tl -> (
        match stack with
        | a :: b :: stl -> aux tl (Int64.add a b :: stl)
        | _ -> failwith "Not enough arguements")
    | Bytecode.SUB :: tl -> (
        match stack with
        | a :: b :: stl -> aux tl (Int64.sub a b :: stl)
        | _ -> failwith "Not enough arguements")
    | Bytecode.MULT :: tl -> (
        match stack with
        | a :: b :: stl -> aux tl (Int64.mul a b :: stl)
        | _ -> failwith "Not enough arguements")
    | Bytecode.DIV :: tl -> (
        match stack with
        | a :: b :: stl -> aux tl (Int64.div a b :: stl)
        | _ -> failwith "Not enough arguements")
    | Bytecode.HALT :: _ -> List.hd stack
    | _ -> failwith "Invalid instruction"
  in
  aux insts []
