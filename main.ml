open Kontlang

let f s =
  Lexing.from_string s
  |> Parser.f Lexer.f
  |> Execute.eval (Builtins.load Env.empty)
  |> Val.to_string
  |> print_endline

let rec read_eval_print exp_string =
  let s = read_line () in
  if s = "" then f exp_string
  else read_eval_print (Printf.sprintf "%s\n%s" exp_string s)

let () = (print_string ">>> "; read_eval_print "")