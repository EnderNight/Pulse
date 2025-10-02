type expression_kind =
  | Int of int64
  | Var of string
  | BinOp of Parsetree.binop * expression * expression

and expression = {
  ty : Types.ty;
  kind : expression_kind;
  loc : Location.t;
}

type statement_kind =
  | Let of string * expression
  | Assign of string * expression
  | IfElse of expression * statement list * statement list option
  | While of expression * statement list
  | Print of expression
  | PrintInt of expression

and statement = {
  kind : statement_kind;
  loc : Location.t;
}

type program = statement list
type generator = { node_acc : int }

let gen_init = { node_acc = 0 }

let gen_node g =
  let node = "node" ^ string_of_int g.node_acc and node_acc = g.node_acc + 1 in
  (node, { node_acc })

let rec expr_to_dot (expr : expression) g =
  match expr.kind with
  | Int n ->
      let node_name, g = gen_node g in
      let node = node_name ^ " [label=\"{Int|" ^ Int64.to_string n ^ "}\"]" in
      (node_name, [ node ], g)
  | Var id ->
      let node_name, g = gen_node g in
      let node =
        node_name ^ " [label=\"{Var|{" ^ expr.ty.name ^ "|" ^ id ^ "}}\"]"
      in
      (node_name, [ node ], g)
  | BinOp (op, lhs, rhs) ->
      let op_dot =
        match op with
        | Plus -> "\\+"
        | Minus -> "\\-"
        | Mult -> "\\*"
        | Div -> "\\/"
        | Mod -> "\\%"
        | Eq -> "\\=="
        | Neq -> "\\!="
        | Lt -> "\\<"
        | Le -> "\\<="
        | Gt -> "\\>="
        | Ge -> "\\>="
      in
      let node_name, g = gen_node g in
      let node =
        node_name ^ " [label=\"{BinOp|{" ^ lhs.ty.name ^ "|" ^ op_dot
        ^ "|<lhs>|<rhs>}}\"]"
      in
      let l_node_name, l_graph, g = expr_to_dot lhs g in
      let r_node_name, r_graph, g = expr_to_dot rhs g in
      let ledge = node_name ^ ":lhs -> " ^ l_node_name
      and redge = node_name ^ ":rhs -> " ^ r_node_name in
      (node_name, [ node; ledge; redge ] @ l_graph @ r_graph, g)

and stmt_to_dot stmt g =
  match stmt.kind with
  | Let (id, expr) ->
      let node_name, g = gen_node g in
      let node =
        node_name ^ " [color=red;label=\"{Let|{" ^ expr.ty.name ^ "|" ^ id
        ^ "|<expr>}}\"]"
      in
      let expr_node_name, expr_graph, g = expr_to_dot expr g in
      let edge = node_name ^ ":expr -> " ^ expr_node_name in
      (node_name, node :: edge :: expr_graph, g)
  | Assign (id, expr) ->
      let node_name, g = gen_node g in
      let node =
        node_name ^ " [color=red;label=\"{Assign|{" ^ expr.ty.name ^ "|" ^ id
        ^ "|<expr>}}\"]"
      in
      let expr_node_name, expr_graph, g = expr_to_dot expr g in
      let edge = node_name ^ ":expr -> " ^ expr_node_name in
      (node_name, node :: edge :: expr_graph, g)
  | Print expr ->
      let node_name, g = gen_node g in
      let node =
        node_name ^ " [color=gold;label=\"{Print|{" ^ expr.ty.name
        ^ "|<expr>}}\"]"
      in
      let expr_node_name, expr_graph, g = expr_to_dot expr g in
      let edge = node_name ^ ":expr -> " ^ expr_node_name in
      (node_name, node :: edge :: expr_graph, g)
  | PrintInt expr ->
      let node_name, g = gen_node g in
      let node =
        node_name ^ " [color=gold;label=\"{PrintInt|{" ^ expr.ty.name
        ^ "|<expr>}}\"]"
      in
      let expr_node_name, expr_graph, g = expr_to_dot expr g in
      let edge = node_name ^ ":expr -> " ^ expr_node_name in
      (node_name, node :: edge :: expr_graph, g)
  | IfElse (cond, btrue, bfalse) ->
      let node_name, g = gen_node g in
      let node =
        node_name ^ " [color=blue;label=\"{IfElse|{<cond>|<btrue>|<bfalse>}}\"]"
      in
      let cond_node_name, cond_graph, g = expr_to_dot cond g in
      let cedge = node_name ^ ":cond -> " ^ cond_node_name in
      let btrue_name, true_graph, g = stmts_to_dot btrue g in
      let tedge = node_name ^ ":btrue -> " ^ btrue_name in
      let false_graph, fedge, g =
        match bfalse with
        | None -> ([], "", g)
        | Some bfalse ->
            let bfalse_name, false_graph, g = stmts_to_dot bfalse g in
            let fedge = node_name ^ ":bfalse -> " ^ bfalse_name in
            (false_graph, fedge, g)
      in
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
