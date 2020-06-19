type valt =
| Nil
| Int of int
| Str of string
| Bool of bool
| Op of string * (valt list -> valt)
| Fn of string * string list * (string * valt) list ref * Exp.t
| Macro of string list * Exp.t
| Cons of valt * valt
| Cont of string * (string * valt) list list * contt list
| Module of (string * valt) list
and contt =
| Call of Exp.t list * valt list
| If of Exp.t * Exp.t
| Cond of Exp.t * (Exp.t * Exp.t) list
| Let of string * (string * Exp.t) list * (string * valt) list * Exp.t
| Lets of string * (string * Exp.t) list * Exp.t
| Do of Exp.t list
| ModuleExp of Exp.t list * (string * valt) list
| ModuleInclude of Exp.t list * (string * valt) list
| ModuleUsing of Exp.t list * (string * valt) list
| ModuleDefine of string * Exp.t list * (string * valt) list
| Import
| Open of Exp.t
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
  | Cont of string * (string * valt) list list * contt list
  | Module of (string * valt) list

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
  | Cont(s, _, _) ->  Printf.sprintf "Cont(%s)" s
  | Module _ -> "Module()"
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
  | ModuleExp of Exp.t list * (string * valt) list
  | ModuleInclude of Exp.t list * (string * valt) list
  | ModuleUsing of Exp.t list * (string * valt) list
  | ModuleDefine of string * Exp.t list * (string * valt) list
  | Import
  | Open of Exp.t
  | Env
  
  type t = cont list list
  
  let final = [[]]
  
  let add c = function
  | cont'::cont'' -> (c::cont')::cont''
  | [] -> [[c]]

  let hd_opt = function
  | [] -> None
  | []::_ -> None
  | (x::_)::_ -> Some x

  let to_string_cont cont =
  let paren x y = Printf.sprintf "(%s %s)" x y in
  let to_string_es es = String.concat " " @@ List.map Exp.to_string es in
  let to_string_vs vs =  String.concat " " @@ List.map Val.to_string vs in
  let to_string_ee (e1, e2) = paren (Exp.to_string e1) (Exp.to_string e2) in
  let to_string_se (s, e) = paren s (Exp.to_string e) in
  let to_string_sv (s, v) = paren s (Val.to_string v) in
  let to_string_ees ees = String.concat " " @@ List.map to_string_ee ees in
  let to_string_ses ses = String.concat " " @@ List.map to_string_se ses in
  let to_string_svs svs = String.concat " " @@ List.map to_string_sv svs in
  match cont with
  | Call(es, vs) -> Printf.sprintf "CALL [%s] [%s]" (to_string_es es) (to_string_vs vs)
  | If(e1, e2) -> Printf.sprintf "IF %s %s" (Exp.to_string e1) (Exp.to_string e2)
  | Cond(e, ees) -> Printf.sprintf "COND %s %s" (Exp.to_string e) (to_string_ees ees)
  | Let(s, ses, svs, e) ->
      let ses_str = to_string_ses ses in
      let svs_str = to_string_svs svs in
      let e_str = Exp.to_string e in
      Printf.sprintf "LET %s [%s] [%s] %s" s ses_str svs_str e_str
  | Lets(s, ses, e) ->
      let ses_str = to_string_ses ses in
      let e_str = Exp.to_string e in
      Printf.sprintf "LETS %s [%s] %s" s ses_str e_str
  | Do(es) -> Printf.sprintf "DO [%s]" @@ to_string_es es
  | ModuleExp(es, svs) ->
      let es_str = to_string_es es in
      let svs_str = to_string_svs svs in
      Printf.sprintf "MODULE_EXP [%s] [%s]" es_str svs_str
  | ModuleInclude(es, svs) ->
      let es_str = to_string_es es in
      let svs_str = to_string_svs svs in
      Printf.sprintf "MODULE_INCLUDE [%s] [%s]" es_str svs_str
  | ModuleUsing(es, svs) ->
      let es_str = to_string_es es in
      let svs_str = to_string_svs svs in
      Printf.sprintf "MODULE_USING [%s] [%s]" es_str svs_str
  | ModuleDefine(s, es, svs) ->
      let es_str = to_string_es es in
      let svs_str = to_string_svs svs in
      Printf.sprintf "MODULE_DEFINE %s [%s] [%s]" s es_str svs_str
  | Import -> "IMPORT"
  | Open(e) -> Printf.sprintf "OPEN %s" (Exp.to_string e)
  | Env -> "ENV"

  let to_string_cont_short = function
  | Call _ -> "CALL"
  | If _ -> "IF"
  | Cond _ -> "COND"
  | Let _ -> "LET"
  | Lets _ -> "LETS"
  | Do _ -> "DO"
  | ModuleExp _ -> "MODULE_EXP"
  | ModuleInclude _ -> "MODULE_INCLUDE"
  | ModuleUsing _ -> "MODULE_USING"
  | ModuleDefine _ -> "MODULE_DEFINE"
  | Import -> "IMPORT"
  | Open _ -> "OPEN"
  | Env -> "ENV"

  let to_string cont =
    let f xs = String.concat " " @@ List.map to_string_cont_short xs in
    String.concat " | " @@ List.map f cont

  let to_string_hd cont = match hd_opt cont with
  | None -> ""
  | Some x -> to_string_cont x

  let pop = function
  | [] -> failwith "Popping empty continuation"
  | cont::cont' -> cont, cont'
end