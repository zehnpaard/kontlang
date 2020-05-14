type t =
| Int of int
| Var of string
| Call of t * t list

let rec to_string = function
| Int n -> string_of_int n
| Var s -> s
| Call (e, []) -> Printf.sprintf "(%s)" @@ to_string e 
| Call (e, es) ->
    let fn = to_string e in
    let args = List.map to_string es |> String.concat " " in
    Printf.sprintf "(%s %s)" fn args