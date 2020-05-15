type t =
| Done of Val.t
| Eval of Env.t * Cont.t * Exp.t
| ApplyCont of Env.t * Cont.t * Val.t

let eval env cont = function
| Exp.Int n -> ApplyCont(env, cont, Val.Int n)
| Exp.Var s -> ApplyCont(env, cont, Env.find s env)
| Exp.Call(e, es) -> Eval(env, Cont.Call(es, []) :: cont, e)
| Exp.If(e1, e2, e3) -> Eval(env, Cont.If(e2, e3) :: cont, e1)

let apply_cont env cont v = match cont with
| [] -> Done v
| Cont.Call([], vs) :: cont' -> (match List.rev (v::vs) with
  | Val.Op(_, fn) :: vs' -> ApplyCont(env, cont', (fn vs'))
  | _ -> failwith "Calling non-op in operator position")
| Cont.Call(e::es, vs) :: cont' -> Eval(env, Cont.Call(es, v::vs) :: cont', e)
| Cont.If(e2, e3) :: cont' -> (match v with
  | Val.Bool b -> Eval(env, cont', if b then e2 else e3)
  | _ -> failwith "Non-boolean in condition position of If expression")

let rec trampoline = function
| Done v -> v
| Eval(env, cont, e) -> trampoline @@ eval env cont e
| ApplyCont(env, cont, v) -> trampoline @@ apply_cont env cont v

let run e = trampoline @@ Eval(Builtins.load Env.empty, Cont.final, e)

let eval_string s =
  Lexing.from_string s
  |> Parser.f Lexer.f
  |> run
  |> Val.to_string