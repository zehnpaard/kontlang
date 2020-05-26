let substitute e ss es =
  let env = List.combine ss es in
  let rec f = function
  | Exp.Int _ as x -> x
  | Exp.Str _ as x -> x
  | Exp.Macro(ss, e) -> Exp.Macro(ss, f e)
  | Exp.Var s as e -> (match List.assoc_opt s env with
    | Some e' -> e'
    | None -> e)
  | Exp.Call(e, es) -> Exp.Call(f e, List.map f es)
  | Exp.If(e1, e2, e3) -> Exp.If(f e1, f e2, f e3)
  | Exp.Cond(ees) ->
      Exp.Cond(List.map (fun (e1, e2) -> (f e1, f e2)) ees)
  | Exp.Let(ves, e2) ->
      Exp.Let(List.map (fun (v, e) -> (v, f e)) ves, f e2)
  | Exp.Lets(ves, e2) ->
      Exp.Lets(List.map (fun (v, e) -> (v, f e)) ves, f e2)
  | Exp.Fn(params, body) -> Exp.Fn(params, f body)
  | Exp.LetFn(fns, e) ->
      Exp.LetFn(List.map (fun (v, ps, e) -> (v, ps, f e)) fns, f e)
  | Exp.LetRec(fns, e) ->
      Exp.LetRec(List.map (fun (v, ps, e) -> (v, ps, f e)) fns, f e)
  | Exp.Do es -> Exp.Do (List.map f es)
in
f e