type t =
| Int of int
| Var of string
| Call of t * t list
| If of t * t * t
| Let of (string * t) list * t
| Lets of (string * t) list * t
| Fn of string list * t

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
| Let (ves, e2) ->
    let vess = to_string_ves ves in
    let s2 = to_string e2 in
    Printf.sprintf "(let [%s] %s)" vess s2
| Lets (ves, e2) ->
    let vess = to_string_ves ves in
    let s2 = to_string e2 in
    Printf.sprintf "(let* [%s] %s)" vess s2
| Fn (params, body) ->
    Printf.sprintf "(fn [%s] %s)" (String.concat " " params) (to_string body)
and to_string_ves ves =
  let f (s, e) = Printf.sprintf "(%s %s)" s (to_string e) in
  List.map f ves |> String.concat " "
