type cont =
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