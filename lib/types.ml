type ty = { name : string }

let primitive_int = { name = "int" }
let is_compatible t1 t2 = String.equal t1.name t2.name
