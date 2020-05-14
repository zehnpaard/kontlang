let rec eval env cont = function
| Exp.Int n -> apply_cont env cont @@ Val.Int n
| Exp.Var s -> apply_cont env cont @@ Env.find s env
| Exp.Call (e, es) -> eval env (Cont.Call (es, []) :: cont) e
and apply_cont env cont v = match cont with
| [] -> v
| Cont.Call ([], vs) :: cont' -> (match List.rev (v::vs) with
  | Val.Op (_, fn) :: vs' -> apply_cont env cont' (fn vs')
  | _ -> failwith "Calling non-op in operator position")
| Cont.Call (e::es, vs) :: cont' -> eval env (Cont.Call (es, v::vs) :: cont') e

let eval_string s =
  Lexing.from_string s
  |> Parser.f Lexer.f
  |> eval (Builtins.load Env.empty) Cont.final
  |> Val.to_string