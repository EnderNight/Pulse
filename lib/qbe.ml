(* This type description does not prevent bad qbe code. *)
(* It is also not complete. *)

type ty =
  | W
  | L

type scope =
  | Global
  | Local

type ident = {
  name : string;
  scope : scope;
}

type operand_kind =
  | Ident of ident
  | Const of int64

type operand = {
  ty : ty;
  kind : operand_kind;
}

type cmp_type =
  | Eq
  | Ne
  | Sle
  | Slt
  | Sge
  | Sgt
  | Ule
  | Ult
  | Uge
  | Ugt

type call = {
  ret : operand option;
  func : string;
  args : operand list;
}

type inst =
  | Add of operand * operand * operand
  | Sub of operand * operand * operand
  | Mul of operand * operand * operand
  | Div of operand * operand * operand
  | Rem of operand * operand * operand
  | Or of operand * operand * operand
  | And of operand * operand * operand
  | Shl of operand * operand * operand
  | Shr of operand * operand * operand
  | Cmp of operand * operand * operand * cmp_type * ty
  | Copy of operand * operand
  | Call of call

type jmp_inst =
  | Jmp of string
  | Jnz of operand * string * string
  | Ret of operand
  | Hlt

type block = {
  label : string;
  instructions : inst list;
  jmp_inst : jmp_inst;
}

type func = {
  exported : bool;
  ret_ty : ty;
  func : string;
  args : operand list;
  body : block list;
}

type program = func list

let rec show_ty ty = match ty with W -> "w" | L -> "l"
and show_scope s = match s with Global -> "$" | Local -> "%"

and show_ident id =
  let scope = show_scope id.scope in
  scope ^ id.name

and show_operand_kind opk =
  match opk with Ident id -> show_ident id | Const c -> Int64.to_string c

and show_operand op =
  let ty = show_ty op.ty and kind = show_operand_kind op.kind in
  ty ^ " " ^ kind

and show_cmp_type cty =
  match cty with
  | Eq -> "eq"
  | Ne -> "ne"
  | Sle -> "sle"
  | Slt -> "slt"
  | Sge -> "sge"
  | Sgt -> "sgt"
  | Ule -> "ule"
  | Ult -> "ult"
  | Uge -> "uge"
  | Ugt -> "ugt"

and show_op_rv op =
  let ty = show_ty op.ty and kind = show_operand_kind op.kind in
  kind ^ " =" ^ ty

and show_call (c : call) =
  let ret = match c.ret with None -> "" | Some ret -> show_op_rv ret ^ " "
  and args = List.map show_operand c.args in
  ret ^ "call " ^ "$" ^ c.func ^ "(" ^ String.concat ", " args ^ ")"

and show_inst inst =
  (* tac => Three Address Code *)
  let show_tac rv lhs rhs inst =
    let rv = show_op_rv rv
    and lhs = show_operand_kind lhs.kind
    and rhs = show_operand_kind rhs.kind in
    rv ^ " " ^ inst ^ " " ^ lhs ^ ", " ^ rhs
  in

  match inst with
  | Add (rv, lhs, rhs) -> show_tac rv lhs rhs "add"
  | Sub (rv, lhs, rhs) -> show_tac rv lhs rhs "sub"
  | Mul (rv, lhs, rhs) -> show_tac rv lhs rhs "mul"
  | Div (rv, lhs, rhs) -> show_tac rv lhs rhs "div"
  | Rem (rv, lhs, rhs) -> show_tac rv lhs rhs "rem"
  | Or (rv, lhs, rhs) -> show_tac rv lhs rhs "or"
  | And (rv, lhs, rhs) -> show_tac rv lhs rhs "and"
  | Shl (rv, lhs, rhs) -> show_tac rv lhs rhs "shl"
  | Shr (rv, lhs, rhs) -> show_tac rv lhs rhs "shr"
  | Cmp (rv, lhs, rhs, cty, opty) ->
      let inst = "c" ^ show_cmp_type cty ^ show_ty opty in
      show_tac rv lhs rhs inst
  | Copy (rv, op) ->
      let rv = show_op_rv rv and op = show_operand_kind op.kind in
      rv ^ " copy " ^ op
  | Call c -> show_call c

and show_label l = "@" ^ l

and show_jmp_inst jmp_inst =
  match jmp_inst with
  | Jmp l -> "jmp " ^ show_label l
  | Jnz (op, lnz, lz) ->
      let op = show_operand_kind op.kind
      and lnz = show_label lnz
      and lz = show_label lz in
      "jnz " ^ op ^ ", " ^ lnz ^ ", " ^ lz
  | Ret op -> "ret " ^ show_operand_kind op.kind
  | Hlt -> "hlt"

and show_block b =
  let label = show_label b.label
  and insts = List.map (fun i -> "    " ^ show_inst i) b.instructions
  and jump = "    " ^ show_jmp_inst b.jmp_inst in
  let block = (label :: insts) @ [ jump ] in
  String.concat "\n" block

and show_func f =
  let export = if f.exported then "export " else ""
  and ret_ty = show_ty f.ret_ty
  and fun_name = "$" ^ f.func
  and args = List.map show_operand f.args |> String.concat ", "
  and body = List.map show_block f.body |> String.concat "\n" in
  export ^ "function " ^ ret_ty ^ " " ^ fun_name ^ "(" ^ args ^ ")" ^ " {\n"
  ^ body ^ "\n}\n\n"

and show_program (prog : program) =
  List.map show_func prog |> String.concat "\n"
