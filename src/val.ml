type t =
| Nil
| Int of int
| Bool of bool
| Op of string * (t list -> t)
| Fn of string * string list * (string * t) list ref * Exp.t
| Cons of t * t

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
| Bool b -> string_of_bool b
| Op(s, _) -> Printf.sprintf "Op(%s)" s
| Fn(s, _, _, _) -> Printf.sprintf "Fn(%s)" s
| Cons _ as v ->
    Printf.sprintf "(%s)" (if is_list v then (to_string_list @@ cons_to_list v)
                           else (to_string_dotted_list @@ cons_to_dotted_list [] v))
and to_string_list vs = String.concat " " @@ List.map to_string vs
and to_string_dotted_list (vs, v) =
  let vs_ = to_string_list vs in
  let v_ = to_string v in
  Printf.sprintf "%s . %s" vs_ v_
