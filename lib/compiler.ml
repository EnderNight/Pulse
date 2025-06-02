let compile tree =
  let rec aux tree inst =
    match tree with
    | Parsetree.Number num -> Bytecode.PUSH num :: inst
    | Parsetree.Plus (l, r) ->
        let inst = aux l inst in
        let inst = aux r inst in
        Bytecode.ADD :: inst
    | Parsetree.Minus (l, r) ->
        let inst = aux l inst in
        let inst = aux r inst in
        Bytecode.SUB :: inst
    | Parsetree.Mult (l, r) ->
        let inst = aux l inst in
        let inst = aux r inst in
        Bytecode.MULT :: inst
    | Parsetree.Div (l, r) ->
        let inst = aux l inst in
        let inst = aux r inst in
        Bytecode.DIV :: inst
  in
  aux tree [] |> List.cons Bytecode.HALT |> List.rev
