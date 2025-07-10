type operand =
  | Const of int64
  | Var of string

type instruction =
  | Add of string * operand * operand
  | Sub of string * operand * operand
  | Mul of string * operand * operand
  | Div of string * operand * operand
  | Print of operand

type program = instruction list

let rec show_ir_operand op =
  match op with Const n -> Int64.to_string n | Var id -> id

and show_ir_instruction inst =
  let show_inst inst tmp lop rop =
    let l = show_ir_operand lop and r = show_ir_operand rop in
    tmp ^ " = " ^ inst ^ " " ^ l ^ ", " ^ r
  in
  let s =
    match inst with
    | Add (tmp, lop, rop) -> show_inst "add" tmp lop rop
    | Sub (tmp, lop, rop) -> show_inst "sub" tmp lop rop
    | Mul (tmp, lop, rop) -> show_inst "mul" tmp lop rop
    | Div (tmp, lop, rop) -> show_inst "div" tmp lop rop
    | Print op ->
        let v = show_ir_operand op in
        "print " ^ v
  in
  s ^ "\n"

and show_ir_program program =
  match program with
  | [] -> ""
  | inst :: tl -> show_ir_instruction inst ^ show_ir_program tl
