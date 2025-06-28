let compile tree =
  let rec aux tree =
    match tree with
    | Parsetree.Int n -> [ Bytecode.PUSH n ]
    | Parsetree.Plus (lhs, rhs) ->
        let l = aux lhs and r = aux rhs in
        Bytecode.ADD :: (l @ r)
    | Parsetree.Minus (lhs, rhs) ->
        let l = aux lhs and r = aux rhs in
        Bytecode.SUB :: (l @ r)
    | Parsetree.Mult (lhs, rhs) ->
        let l = aux lhs and r = aux rhs in
        Bytecode.MULT :: (l @ r)
    | Parsetree.Div (lhs, rhs) ->
        let l = aux lhs and r = aux rhs in
        Bytecode.DIV :: (l @ r)
  in
  List.rev (Bytecode.HALT :: aux tree)
