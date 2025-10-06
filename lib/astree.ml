type binop =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Eq
  | Neq
  | Lt
  | Le
  | Gt
  | Ge
  | Or
  | And
  | Shl
  | Shr

and expression_kind =
  | Number of int64
  | Ident of string
  | ArrayInit of expression
  | ArrayAccess of expression * expression
  | BinOp of binop * expression * expression

and expression = {
  kind : expression_kind;
  loc : Location.t;
}

and statement_kind =
  | Let of string * expression
  | If of expression * block
  | IfElse of expression * block * block
  | While of expression * block
  | Assign of expression * expression
  | Expression of expression
  | Print of expression
  | PrintInt of expression

and statement = {
  kind : statement_kind;
  loc : Location.t;
}

and block = statement list
and program = statement list

type generator = { node_acc : int }

let gen_init = { node_acc = 0 }

let gen_node g =
  let node = "node" ^ string_of_int g.node_acc and node_acc = g.node_acc + 1 in
  (node, { node_acc })

let rec expr_to_dot expr g =
  match expr.kind with
  | Number n ->
      let node_name, g = gen_node g in
      let node =
        node_name ^ " [label=\"{Number|" ^ Int64.to_string n ^ "}\"]"
      in
      (node_name, [ node ], g)
  | Ident id ->
      let node_name, g = gen_node g in
      let node = node_name ^ " [label=\"{Ident|" ^ id ^ "}\"]" in
      (node_name, [ node ], g)
  | ArrayInit expr ->
      let node_name, g = gen_node g in
      let node = node_name ^ " [label=\"{ArrayInit|<expr>}\"]" in
      let enode, einsts, g = expr_to_dot expr g in
      let edge = node_name ^ ":expr -> " ^ enode in
      (node_name, node :: edge :: einsts, g)
  | ArrayAccess (var, index) ->
      let node_name, g = gen_node g in
      let node = node_name ^ " [label=\"{ArrayAccess|{<var>|<index>}}\"]" in
      let vnode, vinsts, g = expr_to_dot var g in
      let inode, iinsts, g = expr_to_dot index g in
      let vedge = node_name ^ ":var -> " ^ vnode
      and iedge = node_name ^ ":index -> " ^ inode in
      (node_name, (node :: vedge :: iedge :: vinsts) @ iinsts, g)
  | BinOp (op, lhs, rhs) ->
      let op_dot =
        match op with
        | Add -> "\\+"
        | Sub -> "\\-"
        | Mul -> "\\*"
        | Div -> "\\/"
        | Mod -> "\\%"
        | Eq -> "\\=="
        | Neq -> "\\!="
        | Lt -> "\\<"
        | Le -> "\\<="
        | Gt -> "\\>="
        | Ge -> "\\>="
        | Or -> "\\|"
        | And -> "\\&"
        | Shl -> "\\<\\<"
        | Shr -> "\\>\\>"
      in
      let node_name, g = gen_node g in
      let node =
        node_name ^ " [label=\"{BinOp|{" ^ op_dot ^ "|<lhs>|<rhs>}}\"]"
      in
      let l_node_name, l_graph, g = expr_to_dot lhs g in
      let r_node_name, r_graph, g = expr_to_dot rhs g in
      let ledge = node_name ^ ":lhs -> " ^ l_node_name
      and redge = node_name ^ ":rhs -> " ^ r_node_name in
      (node_name, [ node; ledge; redge ] @ l_graph @ r_graph, g)

and stmt_to_dot (stmt : statement) g =
  match stmt.kind with
  | Let (id, expr) ->
      let node_name, g = gen_node g in
      let node =
        node_name ^ " [color=red;label=\"{Let|{" ^ id ^ "|<expr>}}\"]"
      in
      let expr_node_name, expr_graph, g = expr_to_dot expr g in
      let edge = node_name ^ ":expr -> " ^ expr_node_name in
      (node_name, node :: edge :: expr_graph, g)
  | If (cond, btrue) ->
      let node_name, g = gen_node g in
      let node = node_name ^ " [color=blue;label=\"{If|{<cond>|<btrue>}}\"]" in
      let cond_node_name, cond_graph, g = expr_to_dot cond g in
      let cedge = node_name ^ ":cond -> " ^ cond_node_name in
      let btrue_name, true_graph, g = stmts_to_dot btrue g in
      let tedge = node_name ^ ":btrue -> " ^ btrue_name in
      (node_name, (node :: cedge :: tedge :: cond_graph) @ true_graph, g)
  | IfElse (cond, btrue, bfalse) ->
      let node_name, g = gen_node g in
      let node =
        node_name ^ " [color=blue;label=\"{IfElse|{<cond>|<btrue>|<bfalse>}}\"]"
      in
      let cond_node_name, cond_graph, g = expr_to_dot cond g in
      let cedge = node_name ^ ":cond -> " ^ cond_node_name in
      let btrue_name, true_graph, g = stmts_to_dot btrue g in
      let tedge = node_name ^ ":btrue -> " ^ btrue_name in
      let bfalse_name, false_graph, g = stmts_to_dot bfalse g in
      let fedge = node_name ^ ":bfalse -> " ^ bfalse_name in
      ( node_name,
        (node :: cedge :: tedge :: fedge :: cond_graph)
        @ true_graph @ false_graph,
        g )
  | While (cond, body) ->
      let node_name, g = gen_node g in
      let node =
        node_name ^ " [color=blue;label=\"{While|{<cond>|<body>}}\"]"
      in
      let cond_node_name, cond_graph, g = expr_to_dot cond g in
      let cedge = node_name ^ ":cond -> " ^ cond_node_name in
      let body_name, body_graph, g = stmts_to_dot body g in
      let bedge = node_name ^ ":body -> " ^ body_name in
      (node_name, (node :: cedge :: bedge :: cond_graph) @ body_graph, g)
  | Assign (var, expr) ->
      let node_name, g = gen_node g in
      let node = node_name ^ " [color=red;label=\"{Assign|{<var>|<expr>}}\"]" in
      let expr_node_name, expr_graph, g = expr_to_dot expr g in
      let eedge = node_name ^ ":expr -> " ^ expr_node_name in
      let var_node_name, var_graph, g = expr_to_dot var g in
      let vedge = node_name ^ ":var -> " ^ var_node_name in
      (node_name, (node :: eedge :: vedge :: var_graph) @ expr_graph, g)
  | Expression expr ->
      let node_name, g = gen_node g in
      let node = node_name ^ " [color=red;label=\"{Expression|<expr>}\"]" in
      let expr_node_name, expr_graph, g = expr_to_dot expr g in
      let eedge = node_name ^ ":expr -> " ^ expr_node_name in
      (node_name, node :: eedge :: expr_graph, g)
  | Print expr ->
      let node_name, g = gen_node g in
      let node = node_name ^ " [color=gold;label=\"{Print|{<expr>}}\"]" in
      let expr_node_name, expr_graph, g = expr_to_dot expr g in
      let edge = node_name ^ ":expr -> " ^ expr_node_name in
      (node_name, node :: edge :: expr_graph, g)
  | PrintInt expr ->
      let node_name, g = gen_node g in
      let node = node_name ^ " [color=gold;label=\"{PrintInt|{<expr>}}\"]" in
      let expr_node_name, expr_graph, g = expr_to_dot expr g in
      let edge = node_name ^ ":expr -> " ^ expr_node_name in
      (node_name, node :: edge :: expr_graph, g)

and stmts_to_dot stmts g =
  let node_name, g = gen_node g in
  let node = node_name ^ "[color=blue;label=\"Stmt Block\"]" in
  let link_edge stmt_node_name graph =
    let edge = node_name ^ " -> " ^ stmt_node_name in
    edge :: graph
  in
  let link_stmt g stmt =
    let stmt_node_name, stmt_graph, g = stmt_to_dot stmt g in
    let stmt_graph = link_edge stmt_node_name stmt_graph in
    (g, stmt_graph)
  in
  let g, graph = List.fold_left_map link_stmt g stmts in
  (node_name, node :: List.concat graph, g)

and program_to_dot program g =
  let _, graph, _ = stmts_to_dot program g in
  let graph = List.map (fun l -> "    " ^ l) graph in
  ("digraph G {" :: "node [shape=record]" :: graph) @ [ "}" ]
  |> String.concat "\n"

let to_dot program = program_to_dot program gen_init
