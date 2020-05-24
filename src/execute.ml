type t =
| Done of Val.t
| Eval of Env.t * Cont.t * Exp.t
| ApplyCont of Env.t * Cont.t * Val.t

let rec get_free env = function
| Exp.Int _ -> []
| Exp.Var s -> if (Env.contains s env) then [] else [s]
| Exp.Call(e, es) -> (get_free env e) @ (List.concat @@ List.map (get_free env) es) 
| Exp.If(e1, e2, e3) -> (get_free env e1) @ (get_free env e2) @ (get_free env e3)
| Exp.Let(ves, e2) ->
    let vs, es = List.split ves in
    (get_free (Env.add_vars env vs) e2) @ (List.concat @@ List.map (get_free env) es)
| Exp.Lets((s, e1)::ves, e2) ->
    (get_free env e1) @ (get_free (Env.add_var env s) @@ Exp.Lets(ves, e2))
| Exp.Lets ([], e2) -> get_free env e2
| Exp.Fn(params, body) -> get_free (Env.add_vars env params) body
| Exp.LetFn(fns, e) ->
    let fnames, paramss, bodys = Utils.split3 fns in
    let f params body = get_free (Env.add_vars env params) body in
    let free_in_fns = List.concat @@ List.map2 f paramss bodys in
    (get_free (Env.add_vars env fnames) e) @ free_in_fns
| Exp.LetRec((fname, params, fbody), e) ->
    let f params fbody = get_free (Env.add_vars env (fname::params)) fbody in
    (get_free (Env.add_var env fname) e) @ f params fbody

let eval env cont = function
| Exp.Int n -> ApplyCont(env, cont, Val.Int n)
| Exp.Var s -> ApplyCont(env, cont, Env.find s env)
| Exp.Call(e, es) -> Eval(env, Cont.Call(es, []) :: cont, e)
| Exp.If(e1, e2, e3) -> Eval(env, Cont.If(e2, e3) :: cont, e1)
| Exp.Let((s, e1)::ves, e2) -> Eval(env, Cont.Let(s, ves, [], e2) :: cont, e1)
| Exp.Let _ -> failwith "Evaluating empty Let"
| Exp.Lets((s, e1)::ves, e2) -> Eval([]::env, Cont.Lets(s, ves, e2) :: cont, e1)
| Exp.Lets _ -> failwith "Evaluating empty Let"
| Exp.Fn(params, body) as e ->
    let free = get_free Env.empty e in
    let fvals = List.map (fun v -> v, Env.find v env) free in
    ApplyCont(env, cont, Val.Fn("anon", params, fvals, body))
| Exp.LetFn(fns, e) ->
    let f (fname, params, body) =
      let free = get_free (Env.add_vars Env.empty params) body in
      let fvals = List.map (fun v -> v, Env.find v env) free in
      (fname, Val.Fn(fname, params, fvals, body))
    in
    Eval(Env.extend_list (List.map f fns) env, Cont.Env::cont, e)
| Exp.LetRec((fname, params, fbody), e) ->
    let f (fname, params, body) =
      let free = get_free (Env.add_vars Env.empty (fname::params)) body in
      let fvals = List.map (fun v -> v, Env.find v env) free in
      let fvalsr = ref fvals in
      let fn = Val.RecFn(fname, params, fvalsr, body) in
      fvalsr := ((fname, fn) :: !fvalsr);
      (fname, fn)
    in
    Eval(Env.extend_list [f (fname, params, fbody)] env, Cont.Env::cont, e)

let apply_cont env cont v = match cont with
| [] -> Done v
| Cont.Call([], vs) :: cont' -> (match List.rev (v::vs) with
  | Val.Op(_, fn) :: vs' -> ApplyCont(env, cont', (fn vs'))
  | Val.Fn(s, ss, fvals, e) :: vs' ->
    let paramcount = List.length ss in
    let argcount = List.length vs' in
    if paramcount = argcount then
      let env' = Env.extend_list (fvals @ (List.combine ss vs')) env in
      Eval(env', Cont.Env::cont', e)
    else failwith @@ Printf.sprintf "Function %s called with incorrect number of args: expected %d received %d" s paramcount argcount
  | Val.RecFn(s, ss, fvalsr, e) :: vs' ->
    let paramcount = List.length ss in
    let argcount = List.length vs' in
    if paramcount = argcount then
      let env' = Env.extend_list (!fvalsr @ (List.combine ss vs')) env in
      Eval(env', Cont.Env::cont', e)
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