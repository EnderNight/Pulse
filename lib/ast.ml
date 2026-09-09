type bin_op = Plus | Minus
type expr = Integer of int64 | BinExpr of bin_op * expr * expr
type stmt = Print of expr
type t = stmt list
