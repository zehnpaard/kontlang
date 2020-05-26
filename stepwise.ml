open Kontlang

let rec read_eval_print exp_string =
  let s = read_line () in
  if s = "" then Execute.eval_string' exp_string |> print_endline
  else read_eval_print (Printf.sprintf "%s\n%s" exp_string s)

let () = (print_string ">>> "; read_eval_print "")