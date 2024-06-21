type expr = 
    | Int of int
    | String of string
    | Var of string
    | Call of string * expr list
    | BinOp of string * expr * expr

type stmt =
    | Let of string * expr
    | If of expr * stmt list * stmt list option
    | While of expr * stmt list
    | Assign of string * expr
    | Return of expr

type param = {
    name: string;
    type_id: string;
}

type fun_dec = {
    name: string;
    params: param list;
    return_type: string;
    body: stmt list;
}

type ast = fun_dec list
