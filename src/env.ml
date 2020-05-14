type t = (string * Val.t) list

let empty = []

let extend var val_ env = (var, val_)::env
let extend_list vvs env = vvs @ env

let rec find var = function
| [] -> failwith @@ Printf.sprintf "Variable %s not found" var
| (var', val')::env' ->
    if var = var' then val'
    else find var env'