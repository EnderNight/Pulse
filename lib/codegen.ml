type generator = {
  op_acc : int;
  label_acc : int;
  label : string;
  block : Qbe.inst list;
  fblocks : Qbe.block list;
}

let rec gen_init =
  { op_acc = 0; label_acc = 0; label = "start"; block = []; fblocks = [] }

and gen_op g =
  let op = "_" ^ string_of_int g.op_acc and op_acc = g.op_acc + 1 in
  (op, { g with op_acc })

and gen_label lname g =
  let label = lname ^ string_of_int g.label_acc
  and label_acc = g.label_acc + 1 in
  (label, { g with label_acc })

and gen_append_inst inst g =
  let block = inst :: g.block in
  { g with block }

and gen_append_insts insts g =
  let block = insts @ g.block in
  { g with block }

and gen_append_block label jmp_inst g =
  let instructions = List.rev g.block in
  let block : Qbe.block = { label = g.label; instructions; jmp_inst } in
  let fblocks = block :: g.fblocks in
  { g with fblocks; block = []; label }

and gen_func_body end_inst g =
  let g = gen_append_block "" end_inst g in
  List.rev g.fblocks

let gen_ident_op name scope ty =
  let ident : Qbe.ident = { name; scope } in
  ({ ty; kind = Qbe.Ident ident } : Qbe.operand)

and gen_const_op value ty = ({ ty; kind = Qbe.Const value } : Qbe.operand)

let gen_qbe_type (ty : Types.ty) =
  match ty.name with "int" -> Qbe.L | _ -> Utils.not_impl "gen_qbe_type"

let rec gen_expr (exp : Typetree.expression) g =
  let ty = gen_qbe_type exp.ty in
  match exp.kind with
  | Typetree.Int c ->
      let op = gen_const_op c ty in
      (op, [], g)
  | Typetree.Var v ->
      let op = gen_ident_op v Qbe.Local ty in
      (op, [], g)
  | Typetree.BinOp (op, lhs, rhs) ->
      let lop, linst, g = gen_expr lhs g in
      let rop, rinst, g = gen_expr rhs g in
      let tmp, g = gen_op g in
      let tmp_op = gen_ident_op tmp Qbe.Local ty in
      let inst =
        match op with
        | Plus -> Qbe.Add (tmp_op, lop, rop)
        | Minus -> Qbe.Sub (tmp_op, lop, rop)
        | Mult -> Qbe.Mul (tmp_op, lop, rop)
        | Div -> Qbe.Div (tmp_op, lop, rop)
        | Mod -> Qbe.Rem (tmp_op, lop, rop)
        (* lop and rop should have the same type *)
        | Eq -> Qbe.Cmp (tmp_op, lop, rop, Qbe.Eq, lop.ty)
        | Neq -> Qbe.Cmp (tmp_op, lop, rop, Qbe.Ne, lop.ty)
        | Lt -> Qbe.Cmp (tmp_op, lop, rop, Qbe.Slt, lop.ty)
        | Le -> Qbe.Cmp (tmp_op, lop, rop, Qbe.Sle, lop.ty)
        | Gt -> Qbe.Cmp (tmp_op, lop, rop, Qbe.Sgt, lop.ty)
        | Ge -> Qbe.Cmp (tmp_op, lop, rop, Qbe.Sge, lop.ty)
      in
      (tmp_op, (inst :: rinst) @ linst, g)

and gen_statement g (stmt : Typetree.statement) =
  match stmt.kind with
  | Typetree.Let (id, expr) ->
      let tmp_op, insts, g = gen_expr expr g in
      let op = gen_ident_op id Qbe.Local tmp_op.ty in
      let copy_inst = Qbe.Copy (op, tmp_op) in
      gen_append_insts (copy_inst :: insts) g
  | Typetree.Assign (id, expr) ->
      let tmp_op, insts, g = gen_expr expr g in
      let op = gen_ident_op id Qbe.Local tmp_op.ty in
      let copy_inst = Qbe.Copy (op, tmp_op) in
      gen_append_insts (copy_inst :: insts) g
  | Typetree.Print expr ->
      let tmp_op, insts, g = gen_expr expr g in
      let call_inst =
        Qbe.Call { ret = None; func = "print"; args = [ tmp_op ] }
      in
      gen_append_insts (call_inst :: insts) g
  | Typetree.PrintInt expr ->
      let tmp_op, insts, g = gen_expr expr g in
      let call_inst =
        Qbe.Call { ret = None; func = "print_int"; args = [ tmp_op ] }
      in
      gen_append_insts (call_inst :: insts) g
  | Typetree.IfElse (cond, btrue, bfalse) ->
      let cond_lbl, g = gen_label "if.cond" g in
      let true_lbl, g = gen_label "if.true" g in
      let false_lbl, g = gen_label "if.false" g in
      let end_lbl, g = gen_label "if.end" g in
      let cond_op, cond_insts, g = gen_expr cond g in
      gen_append_block cond_lbl (Qbe.Jmp cond_lbl) g
      |> gen_append_insts cond_insts
      |> gen_append_block true_lbl (Qbe.Jnz (cond_op, true_lbl, false_lbl))
      |> gen_statement_block btrue
      |> gen_append_block false_lbl (Qbe.Jmp end_lbl)
      |> (fun g ->
      Option.fold ~none:g
        ~some:(fun bfalse -> gen_statement_block bfalse g)
        bfalse)
      |> gen_append_block end_lbl (Qbe.Jmp end_lbl)
  | Typetree.While (cond, body) ->
      let cond_lbl, g = gen_label "while.cond" g in
      let body_lbl, g = gen_label "while.body" g in
      let end_lbl, g = gen_label "while.end" g in
      let cond_op, cond_insts, g = gen_expr cond g in
      gen_append_block cond_lbl (Qbe.Jmp cond_lbl) g
      |> gen_append_insts cond_insts
      |> gen_append_block body_lbl (Qbe.Jnz (cond_op, body_lbl, end_lbl))
      |> gen_statement_block body
      |> gen_append_block end_lbl (Qbe.Jmp cond_lbl)

and gen_statement_block block g = List.fold_left gen_statement g block

and gen_program (prog : Typetree.program) =
  let g = gen_init in
  let ret_op = gen_const_op Int64.zero Qbe.W in
  let ret_inst = Qbe.Ret ret_op in
  let blocks = List.fold_left gen_statement g prog |> gen_func_body ret_inst in
  let func : Qbe.func =
    { exported = true; ret_ty = Qbe.W; func = "main"; args = []; body = blocks }
  in
  [ func ]
