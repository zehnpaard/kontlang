type valt =
| Nil
| Int of int
| Str of string
| Bool of bool
| Op of string * (valt list -> valt)
| Fn of string * string list * (string * valt) list ref * Exp.t
| Macro of string list * Exp.t
| Cons of valt * valt
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