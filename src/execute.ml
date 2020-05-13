let rec eval env = function
| Exp.Int n -> Val.Int n
| Exp.Var s -> Env.find s env
| Exp.Call (e, es) -> (match eval env e with
  | Val.Op (_, fn) -> fn @@ List.map (eval env) es
  | v ->
      let s = Val.to_string v in
      failwith @@ Printf.sprintf "Non-function %s in operator position" s)