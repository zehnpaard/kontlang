type t =
| Int of int
| Var of string
| Call of t * t list
| If of t * t * t
| Let of string * t * t

let rec to_string = function
| Int n -> string_of_int n
| Var s -> s
| Call (e, []) -> Printf.sprintf "(%s)" @@ to_string e 
| Call (e, es) ->
    let fn = to_string e in
    let args = List.map to_string es |> String.concat " " in
    Printf.sprintf "(%s %s)" fn args
| If (e1, e2, e3) ->
    let s1 = (to_string e1) in
    let s2 = (to_string e2) in
    let s3 = (to_string e3) in
    Printf.sprintf "(if %s %s %s)" s1 s2 s3
| Let (v1, e1, e2) ->
    let s1 = to_string e1 in
    let s2 = to_string e2 in
    Printf.sprintf "(let %s %s %s)" v1 s1 s2
