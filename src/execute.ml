type t =
| Done of Val.t
| Eval of Env.t * Cont.t * Exp.t
| ApplyCont of Env.t * Cont.t * Val.t

let make_closure env free =
  let rec f acc = function
  | [] -> List.rev acc
  | s::ss -> (match Env.find_opt s env with
    | None -> f acc ss
    | Some v -> f ((s, v)::acc) ss)
  in
  f [] free

let rec tco env = function
| (Cont.Env::cont)::cont' -> tco (Env.rest env) @@ cont::cont'
| cont -> env, cont

let eval env cont = function
| Exp.Int n -> ApplyCont(env, cont, Val.Int n)
| Exp.Var s -> ApplyCont(env, cont, Env.find s env)
| Exp.MVar([]) -> failwith "Evaluating invalid/empty module var"
| Exp.MVar(s::ss) ->
  let m = Env.find s env in
  let f m s = match m with
  | Val.Module svs -> (match List.assoc_opt s svs with
    | Some n -> n
    | None -> failwith @@ Printf.sprintf "Member %s of module does not exist" s)
  | _ -> failwith @@ "Accessing member of non-module"
  in
  let v = List.fold_left f m ss in
  ApplyCont(env, cont, v)
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
| Exp.Reset e ->
  let free = Utils.dedupe @@ Exp.get_free [] [] e in
  let fvals = List.map (fun v -> v, Env.find v env) free in
  Eval(Env.extend_list fvals env, []::cont, e)
| Exp.Shift(s, e) ->
  let free = Utils.dedupe @@ Exp.get_free [s] [] e in
  let fvals = List.map (fun v -> v, Env.find v env) free in
  let cont', cont'' = Cont.pop cont in
  let n = 1 + Utils.count Cont.Env cont' in
  let env', env'' = Utils.break_off n env in
  let contv = Val.Cont(s, env', cont') in
  let closure = (s, contv)::fvals in
  Eval(Env.extend_list closure env'', []::cont'', e)
| Exp.Define _ -> failwith "Define used outside of Module definition"
| Exp.Module(Exp.Define(s, e)::es) ->
  let env' = []::env in
  let cont' = Cont.add (Cont.ModuleDefine(s, es, [])) @@ Cont.add Cont.Env cont in
  Eval(env', cont', e)
| Exp.Module(e::es) ->
  let env' = []::env in
  let cont' = Cont.add (Cont.ModuleExp(es, [])) @@ Cont.add Cont.Env cont in
  Eval(env', cont', e)
| Exp.Module [] -> ApplyCont(env, cont, Val.Module [])
| Exp.Import e -> Eval(env, Cont.add Cont.Import cont, e)


let apply_cont env cont v = match cont with
| [] -> Done v
| []::cont'' -> ApplyCont((Env.rest env), cont'', v)
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
      let env', cont''' = tco env @@ cont'::cont'' in
      let env'' = Env.extend_list (!fvalsr @ (List.combine ss vs')) env' in
      Eval(env'', Cont.add Cont.Env cont''', e)
    else failwith @@ Printf.sprintf "Function %s called with incorrect number of args: expected %d received %d" s paramcount argcount
  | Val.Cont(s, svss, cont''') :: vs' ->
    let argcount = List.length vs' in
    if argcount = 1 then
      let final_cont = cont'''::cont'::cont'' in
      let f env svs = Env.extend_list svs env in
      let env' = List.fold_left f env (List.rev svss) in
      ApplyCont(env', final_cont, v)
    else failwith @@ Printf.sprintf "Continuation %s called with incorrect number of args: expected 1 received %d" s argcount
  | vs' -> failwith @@ Printf.sprintf "Calling non-callable %s in operator position" (Val.to_string @@ List.hd vs'))
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
| (Cont.ModuleDefine(s, [], svs)::cont')::cont'' ->
    ApplyCont(env, cont'::cont'', Val.Module((s, v)::svs))
| (Cont.ModuleDefine(s, Exp.Define(s', e')::es, svs)::cont')::cont'' ->
    let env' = Env.extend_current s v env in
    let cont''' = Cont.add (Cont.ModuleDefine(s', es, (s,v)::svs)) (cont'::cont'')in
    Eval(env', cont''', e')
| (Cont.ModuleDefine(s, e::es, svs)::cont')::cont'' ->
    let env' = Env.extend_current s v env in
    let cont''' = Cont.add (Cont.ModuleExp(es, (s,v)::svs)) (cont'::cont'')in
    Eval(env', cont''', e)
| (Cont.ModuleExp([], svs)::cont')::cont'' ->
    ApplyCont(env, cont'::cont'', Val.Module svs)
| (Cont.ModuleExp(Exp.Define(s', e')::es, svs)::cont')::cont'' ->
    let cont''' = Cont.add (Cont.ModuleDefine(s', es, svs)) (cont'::cont'')in
    Eval(env, cont''', e')
| (Cont.ModuleExp(e::es, svs)::cont')::cont'' ->
    let cont''' = Cont.add (Cont.ModuleExp(es, svs)) (cont'::cont'')in
    Eval(env, cont''', e)
| (Cont.Import :: cont')::cont'' -> (match v with
  | Val.Str s ->
    let s' = Std.input_all (open_in s) in
    let s'' = Printf.sprintf "(module [%s])" s' in
    let e = Parser.f Lexer.f @@ Lexing.from_string s'' in
    Eval(env, cont'::cont'', e)
  | _ -> failwith "Non-string passed to Import")
| (Cont.Env :: cont')::cont'' -> ApplyCont (Env.rest env, cont'::cont'', v)

let wrap_s s =
  Printf.sprintf
  "(let* [(Config (import \"config.ktl\"))
          (Stdlib (import (concat Config.stdlib_path \"stdlib.ktl\")))]
     (reset %s))" s

let rec trampoline = function
| Done v -> v
| Eval(env, cont, e) -> trampoline @@ eval env cont e
| ApplyCont(env, cont, v) -> trampoline @@ apply_cont env cont v

let run e = trampoline @@ Eval(Builtins.load Env.empty, Cont.final, e)

let eval_string s =
  wrap_s s
  |> Lexing.from_string
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
  wrap_s s
  |> Lexing.from_string
  |> Parser.f Lexer.f
  |> run'
  |> Val.to_string