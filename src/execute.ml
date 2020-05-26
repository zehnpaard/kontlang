type t =
| Done of Val.t
| Eval of Env.t * Cont.t * Exp.t
| ApplyCont of Env.t * Cont.t * Val.t

let eval env cont = function
| Exp.Int n -> ApplyCont(env, cont, Val.Int n)
| Exp.Var s -> ApplyCont(env, cont, Env.find s env)
| Exp.Str s -> ApplyCont(env, cont, Val.Str s)
| Exp.Call(e, es) ->
  let cont' = Cont.add (Cont.Call(es, [])) cont in
  Eval(env, cont', e)
| Exp.If(e1, e2, e3) ->
  let cont' = Cont.add (Cont.If(e2, e3)) cont in
  Eval(env, cont', e1)
| Exp.Cond((e1,e2)::ees) ->
  let cont' = Cont.add (Cont.Cond(e2, ees)) cont in
  Eval(env, cont', e1)
| Exp.Cond _ -> failwith "Evaluating cond with no matching condition"
| Exp.Let((s, e1)::ves, e2) ->
  let cont' = Cont.add (Cont.Let(s, ves, [], e2)) cont in
  Eval(env, cont', e1)
| Exp.Let _ -> failwith "Evaluating empty Let"
| Exp.Lets((s, e1)::ves, e2) ->
  let cont' = Cont.add (Cont.Lets(s, ves, e2)) @@ Cont.add Cont.Env cont in
  Eval([]::env, cont', e1)
| Exp.Lets _ -> failwith "Evaluating empty Let"
| Exp.Fn(params, body) as e ->
    let free = Utils.dedupe @@ Exp.get_free [] [] e in
    let fvalsr = ref @@ List.map (fun v -> v, Env.find v env) free in
    ApplyCont(env, cont, Val.Fn("anon", params, fvalsr, body))
| Exp.LetFn(fns, e) ->
    let f (fname, params, body) =
      let free = Utils.dedupe @@ Exp.get_free params [] body in
      let fvalsr = ref @@ List.map (fun v -> v, Env.find v env) free in
      (fname, Val.Fn(fname, params, fvalsr, body))
    in
    Eval(Env.extend_list (List.map f fns) env, Cont.add Cont.Env cont, e)
| Exp.LetRec(fns, e) ->
    let fnames, _, _ = Utils.split3 fns in
    let f (fname, params, body) =
      let free = Utils.dedupe @@ Exp.get_free (fnames @ params) [] body in
      let fvals = List.map (fun v -> v, Env.find v env) free in
      let fvalsr = ref fvals in
      let fn = Val.Fn(fname, params, fvalsr, body) in
      ((fname, fn), fvalsr)
    in
    let fname_fns, fvalsrs = List.split @@ List.map f fns in
    List.iter (fun fvalsr -> (fvalsr := (fname_fns @ !fvalsr))) fvalsrs;
    Eval(Env.extend_list fname_fns env, Cont.add Cont.Env cont, e)
  | Exp.Macro(ss, e) -> ApplyCont(env, cont, Val.Macro(ss, e))
  | Exp.Do(e::es) ->
    let cont' = Cont.add (Cont.Do es) cont in
    Eval(env, cont', e)
  | Exp.Do([]) -> failwith "Evaluating empty do"

let apply_cont env cont v = match cont with
| [] -> Done v
| []::cont'' -> ApplyCont(env, cont'', v)
| (Cont.Call(e::es as es', [])::cont')::cont'' -> (match v with
  | Val.Macro (ss, me) ->
      let paramcount = List.length ss in
      let argcount = List.length es' in
      if paramcount = argcount then
        Eval(env, cont'::cont'', Macro.substitute me ss es')
      else failwith @@ Printf.sprintf "Macro called with incorrect number of args: expected %d received %d" paramcount argcount
  | _ ->
    let cont''' = Cont.add (Cont.Call(es, [v])) (cont'::cont'') in
    Eval(env, cont''', e))
| (Cont.Call(e::es, vs) :: cont')::cont'' ->
    let cont''' = Cont.add (Cont.Call(es, v::vs)) (cont'::cont'') in
    Eval(env, cont''', e)
| (Cont.Call([], vs) :: cont')::cont'' -> (match List.rev (v::vs) with
  | Val.Op(_, fn) :: vs' -> ApplyCont(env, cont'::cont'', (fn vs'))
  | Val.Fn(s, ss, fvalsr, e) :: vs' ->
    let paramcount = List.length ss in
    let argcount = List.length vs' in
    if paramcount = argcount then
      let env' = Env.extend_list (!fvalsr @ (List.combine ss vs')) env in
      Eval(env', Cont.add Cont.Env (cont'::cont''), e)
    else failwith @@ Printf.sprintf "Function %s called with incorrect number of args: expected %d received %d" s paramcount argcount
  | _ -> failwith "Calling non-callable in operator position")
| (Cont.If(e2, e3) :: cont')::cont'' -> (match v with
  | Val.Bool b -> Eval(env, cont'::cont'', if b then e2 else e3)
  | _ -> failwith "Non-boolean in condition position of If expression")
| (Cont.Cond(e, ees) :: cont')::cont'' -> (match v with
  | Val.Bool b -> Eval(env, cont'::cont'', if b then e else Exp.Cond(ees))
  | _ -> failwith "Non-boolean in condition position of Cond expression")
| (Cont.Let(s, [], vvs, e2) :: cont')::cont'' ->
    let env' = Env.extend_list ((s, v)::vvs) env in
    Eval(env', Cont.add Cont.Env (cont'::cont''), e2)
| (Cont.Let(s, (s', e')::ves, vvs, e2) :: cont')::cont'' ->
    let cont''' = Cont.add (Cont.Let(s', ves, (s, v)::vvs, e2)) (cont'::cont'') in
    Eval(env, cont''', e')
| (Cont.Lets(s, [], e2) :: cont')::cont'' ->
    Eval(Env.extend_current s v env, (cont'::cont''), e2)
| (Cont.Lets(s, (s', e')::ves, e2) :: cont')::cont'' ->
    let cont''' = Cont.add (Cont.Lets(s', ves, e2)) (cont'::cont'') in
    Eval(Env.extend_current s v env, cont''', e')
| (Cont.Do(e::es)::cont')::cont'' ->
    let cont''' = Cont.add (Cont.Do es) (cont'::cont'') in
    Eval(env, cont''', e)
| (Cont.Do([])::cont')::cont'' -> ApplyCont(env, cont'::cont'', v)
| (Cont.Env :: cont')::cont'' -> ApplyCont (Env.pop env, cont'::cont'', v)

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

let display = function
| Done v ->
  print_endline "Done";
  print_endline (Val.to_string v);
  ignore @@ read_line ()
| Eval(env, cont, e) ->
  print_endline @@ "Evaluating expression " ^ Exp.to_string e;
  print_endline @@ "Cont Head:\t" ^ Cont.to_string_hd cont;
  print_endline  @@ "Cont Full:\t" ^ Cont.to_string cont;
  print_endline  @@ "Environment:\t" ^ Env.to_string env;
  ignore @@ read_line ()
| ApplyCont(env, cont, v) ->
  print_endline  @@ "Applying continuation on value " ^ Val.to_string v;
  print_endline @@ "Cont Head:\t" ^ Cont.to_string_hd cont;
  print_endline  @@ "Cont Full:\t" ^ Cont.to_string cont;
  print_endline  @@ "Environment:\t" ^ Env.to_string env;
  ignore @@ read_line ()

let rec trampoline' res = match res with
| Done v -> display res; v
| Eval(env, cont, e) -> display res; trampoline' @@ eval env cont e
| ApplyCont(env, cont, v) -> display res; trampoline' @@ apply_cont env cont v

let run' e = trampoline' @@ Eval(Builtins.load Env.empty, Cont.final, e)

let eval_string' s =
  Lexing.from_string s
  |> Parser.f Lexer.f
  |> run'
  |> Val.to_string