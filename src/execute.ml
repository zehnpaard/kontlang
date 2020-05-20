type t =
| Done of Val.t
| Eval of Env.t * Cont.t * Exp.t
| ApplyCont of Env.t * Cont.t * Val.t

let eval env cont = function
| Exp.Int n -> ApplyCont(env, cont, Val.Int n)
| Exp.Var s -> ApplyCont(env, cont, Env.find s env)
| Exp.Call(e, es) -> Eval(env, Cont.Call(es, []) :: cont, e)
| Exp.If(e1, e2, e3) -> Eval(env, Cont.If(e2, e3) :: cont, e1)
| Exp.Let((s, e1)::ves, e2) -> Eval(env, Cont.Let(s, ves, [], e2) :: cont, e1)
| Exp.Let _ -> failwith "Evaluating empty Let"
| Exp.Lets((s, e1)::ves, e2) -> Eval([]::env, Cont.Lets(s, ves, e2) :: cont, e1)
| Exp.Lets _ -> failwith "Evaluating empty Let"
| Exp.Fn(args, body) -> ApplyCont(env, cont, Val.Fn("anon", args, body))

let apply_cont env cont v = match cont with
| [] -> Done v
| Cont.Call([], vs) :: cont' -> (match List.rev (v::vs) with
  | Val.Op(_, fn) :: vs' -> ApplyCont(env, cont', (fn vs'))
  | Val.Fn(s, ss, e) :: vs' ->
    let paramcount = List.length ss in
    let argcount = List.length vs' in
    if paramcount = argcount then Eval(Env.extend_list (List.combine ss vs') env, Cont.Env::cont', e)
    else failwith @@ Printf.sprintf "Function %s called with incorrect number of args: expected %d received %d" s paramcount argcount
  | _ -> failwith "Calling non-callable in operator position")
| Cont.Call(e::es, vs) :: cont' -> Eval(env, Cont.Call(es, v::vs) :: cont', e)
| Cont.If(e2, e3) :: cont' -> (match v with
  | Val.Bool b -> Eval(env, cont', if b then e2 else e3)
  | _ -> failwith "Non-boolean in condition position of If expression")
| Cont.Let(s, [], vvs, e2) :: cont' ->
    let env' = Env.extend_list (List.rev ((s, v)::vvs)) env in
    Eval(env', Cont.Env::cont', e2)
| Cont.Let(s, (s', e')::ves, vvs, e2) :: cont' ->
    Eval(env, Cont.Let(s', ves, (s, v)::vvs, e2) :: cont', e')
| Cont.Lets(s, [], e2) :: cont' ->
    Eval(Env.extend_current s v env, Cont.Env::cont', e2)
| Cont.Lets(s, (s', e')::ves, e2) :: cont' ->
    Eval(Env.extend_current s v env, Cont.Lets(s', ves, e2) :: cont', e')
| Cont.Env :: cont' -> ApplyCont (Env.pop env, cont', v)

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