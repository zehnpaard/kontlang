type t =
| Int of int
| Var of string
| Call of t * t list
| If of t * t * t
| Cond of (t * t) list
| Let of (string * t) list * t
| Lets of (string * t) list * t
| Fn of string list * t
| LetFn of (string * string list * t) list * t
| LetRec of (string * string list * t) list * t

let rec to_string = function
| Int n -> string_of_int n
| Var s -> s
| Call(e, []) -> Printf.sprintf "(%s)" @@ to_string e 
| Call(e, es) ->
    let fn = to_string e in
    let args = List.map to_string es |> String.concat " " in
    Printf.sprintf "(%s %s)" fn args
| If(e1, e2, e3) ->
    let s1 = (to_string e1) in
    let s2 = (to_string e2) in
    let s3 = (to_string e3) in
    Printf.sprintf "(if %s %s %s)" s1 s2 s3
| Cond(ees) ->
    let f (e1, e2) = Printf.sprintf "[%s %s]" (to_string e1) (to_string e2) in
    Printf.sprintf "(cond %s)" (String.concat " " @@ List.map f ees)
| Let(ves, e2) ->
    let vess = to_string_ves ves in
    let s2 = to_string e2 in
    Printf.sprintf "(let [%s] %s)" vess s2
| Lets(ves, e2) ->
    let vess = to_string_ves ves in
    let s2 = to_string e2 in
    Printf.sprintf "(let* [%s] %s)" vess s2
| Fn(params, body) ->
    Printf.sprintf "(fn [%s] %s)" (String.concat " " params) (to_string body)
| LetFn(fns, e) ->
    let fns_s = to_string_fns fns in
    let exp_s = to_string e in
    Printf.sprintf "(letfn [%s] %s)" fns_s exp_s
| LetRec(fns, e) ->
    let fns_s = to_string_fns fns in
    let exp_s = to_string e in
    Printf.sprintf "(letfn [%s] %s)" fns_s exp_s
and to_string_ves ves =
  let f (s, e) = Printf.sprintf "(%s %s)" s (to_string e) in
  List.map f ves |> String.concat " "
and to_string_fns fns =
  let f (fname, params, body) = 
    let params_s = String.concat " " params in
    let body_s = to_string body in
    Printf.sprintf "(%s [%s] %s)" fname params_s body_s
  in
  String.concat " " @@ List.map f fns

let rec get_free bound free = function
| Int _ -> free
| Var s -> if (List.mem s bound) then free else s::free
| Call(e, es) ->
    let es' = e::es in
    List.fold_left (get_free bound) free es'
| If(e1, e2, e3) ->
    let es' = [e1; e2; e3] in
    List.fold_left (get_free bound) free es'
| Cond(ees) ->
    let f free (e1, e2) = get_free bound (get_free bound free e1) e2 in
    List.fold_left f free ees
| Let(ves, e2) ->
    let vs, es = List.split ves in
    let free' = List.fold_left (get_free bound) free es in
    get_free (vs @ bound) free' e2
| Lets((s, e1)::ves, e2) ->
    let free' = get_free bound free e1 in
    get_free (s::bound) free' @@ Lets(ves, e2)
| Lets ([], e2) -> get_free bound free e2
| Fn(params, body) -> get_free (params @ bound) free body
| LetFn(fns, e) ->
    let fnames, paramss, bodys = Utils.split3 fns in
    let f free params body = get_free (params @ bound) free body in
    let free' = List.fold_left2 f free paramss bodys in
    get_free (fnames @ bound) free' e
| LetRec(fns, e) ->
    let fnames, paramss, bodys = Utils.split3 fns in
    let f free params body = get_free (fnames @ params @ bound) free body in
    let free' = List.fold_left2 f free paramss bodys in
    get_free (fnames @ bound) free' e