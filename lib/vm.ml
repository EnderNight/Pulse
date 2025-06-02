let run inst =
  let rec aux inst stack =
    match inst with
    | Bytecode.ADD :: inst_tl -> (
        match stack with
        | a :: b :: stack_tl -> aux inst_tl (Int64.add b a :: stack_tl)
        | _ -> failwith "Not enough operands")
    | Bytecode.SUB :: inst_tl -> (
        match stack with
        | a :: b :: stack_tl -> aux inst_tl (Int64.sub b a :: stack_tl)
        | _ -> failwith "Not enough operands")
    | Bytecode.MULT :: inst_tl -> (
        match stack with
        | a :: b :: stack_tl -> aux inst_tl (Int64.mul b a :: stack_tl)
        | _ -> failwith "Not enough operands")
    | Bytecode.DIV :: inst_tl -> (
        match stack with
        | a :: b :: stack_tl -> aux inst_tl (Int64.div b a :: stack_tl)
        | _ -> failwith "Not enough operands")
    | Bytecode.PUSH num :: inst_tl -> aux inst_tl (num :: stack)
    | Bytecode.HALT :: [] -> (
        match stack with num :: [] -> num | _ -> failwith "Stack is not empty")
    | _ -> failwith "Invalid instruction"
  in
  aux inst []
