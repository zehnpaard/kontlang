type t = (string * Val.t) list list

let empty = []

let extend var val_ env = [(var, val_)]::env
let extend_current var val_ = function
| [] -> [[(var, val_)]]
| env::env' -> ((var, val_)::env)::env'
let extend_list vvs env = vvs :: env
let extend_list_current vvs = function
| [] -> [vvs]
|env::env' -> (vvs @ env)::env'

let rec find var = function
| [] -> failwith @@ Printf.sprintf "Variable %s not found" var
| []::env' -> find var env'
| ((var', val')::env')::env'' ->
    if var = var' then val'
    else find var @@ env'::env''

let rec find_opt var = function
| [] -> None
| []::env' -> find_opt var env'
| ((var', val')::env')::env'' ->
    if var = var' then Some val'
    else find_opt var @@ env'::env''

let pop = function
| [] -> failwith "Popping empty environment"
| svs::env -> svs, env

let rest = function
| [] -> failwith "Taking rest of empty environment"
| _::env' -> env'

let rec contains var = function
| [] -> false
| []::env' -> contains var env'
| ((var', _)::env')::env'' ->
    if var = var' then true
    else contains var @@ env'::env''

let add_var env v = extend_current v (Val.Int 0) env
let add_vars env vs =
  let vvs = List.map (fun v -> (v, Val.Int 0)) vs in
  extend_list vvs env

let to_string env =
  let f (s, v) = Printf.sprintf "{%s %s}" s (Val.to_string v) in
  let g svs = Printf.sprintf "[%s]" @@ String.concat " " @@ List.map f svs in
  String.concat " | " @@ List.map g @@ Utils.drop_last env