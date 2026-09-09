type bin_op = Plus | Minus | Mul | Div | Mod
type expr = Integer of int64 | BinExpr of bin_op * expr * expr
type stmt = Print of expr
type t = stmt list
