type valt =
| Nil
| Int of int
| Str of string
| Bool of bool
| Op of string * (valt list -> valt)
| Fn of string * string list * (string * valt) list ref * Exp.t
| Macro of string list * Exp.t
| Cons of valt * valt
and envt = (string * valt) list list
and contt =
| Call of Exp.t list * valt list
| If of Exp.t * Exp.t
| Cond of Exp.t * (Exp.t * Exp.t) list
| Let of string * (string * Exp.t) list * (string * valt) list * Exp.t
| Lets of string * (string * Exp.t) list * Exp.t
| Do of Exp.t list
| Env

module Val = struct
  type t = valt =
  | Nil
  | Int of int
  | Str of string
  | Bool of bool
  | Op of string * (valt list -> valt)
  | Fn of string * string list * (string * valt) list ref * Exp.t
  | Macro of string list * Exp.t
  | Cons of valt * valt

  let rec is_list = function
  | Nil -> true
  | Cons(_, x) -> is_list x
  | _ -> false
  
  let rec cons_to_list = function
  | Nil -> []
  | Cons(x, xs) -> x::(cons_to_list xs)
  | _ -> failwith "Converting non-cons-list into arg list"
  
  let rec cons_to_dotted_list acc = function
  | Nil -> failwith "Dotted list cannot end with nil"
  | Cons(x, xs) -> cons_to_dotted_list (x::acc) xs
  | v -> (List.rev acc, v)
  
  let rec to_string = function
  | Nil -> "nil"
  | Int n -> string_of_int n
  | Str s -> Printf.sprintf "\"%s\"" s
  | Bool b -> string_of_bool b
  | Op(s, _) -> Printf.sprintf "Op(%s)" s
  | Fn(s, _, _, _) -> Printf.sprintf "Fn(%s)" s
  | Macro _ -> "Macro()"
  | Cons _ as v ->
      Printf.sprintf "(%s)" (if is_list v then (to_string_list @@ cons_to_list v)
                             else (to_string_dotted_list @@ cons_to_dotted_list [] v))
  and to_string_list vs = String.concat " " @@ List.map to_string vs
  and to_string_dotted_list (vs, v) =
    let vs_ = to_string_list vs in
    let v_ = to_string v in
    Printf.sprintf "%s . %s" vs_ v_
end

module Env = struct
  type t = envt

  let empty = [[]]
  
  let extend var val_ env = [(var, val_)]::env
  let extend_current var val_ = function
  | [] -> [[(var, val_)]]
  | env::env' -> ((var, val_)::env)::env'
  let extend_list vvs env = vvs :: env
  
  let rec find var = function
  | [] -> failwith @@ Printf.sprintf "Variable %s not found" var
  | []::env' -> find var env'
  | ((var', val')::env')::env'' ->
      if var = var' then val'
      else find var @@ env'::env''
  
  let pop = function
  | [] -> failwith "Popping empty environment"
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
  
end

module Cont = struct
  type cont = contt =
  | Call of Exp.t list * Val.t list
  | If of Exp.t * Exp.t
  | Cond of Exp.t * (Exp.t * Exp.t) list
  | Let of string * (string * Exp.t) list * (string * Val.t) list * Exp.t
  | Lets of string * (string * Exp.t) list * Exp.t
  | Do of Exp.t list
  | Env
  
  type t = cont list list
  
  let final = [[]]
  
  let add c = function
  | cont'::cont'' -> (c::cont')::cont''
  | [] -> [[c]]
end