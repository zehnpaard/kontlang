open OUnit2
open Kontlang

let () = Unix.chdir "../../.."

let test_macro1 _ =
  let s = "((macro [x] nil) (print (read)))" in
  assert_equal (Execute.eval_string s) "nil"

let test_macro_dynamic1 _ =
  let s = "(let [m (macro [x] (+ x y))]
             (let [y 10] (m 5)))" in
  assert_equal (Execute.eval_string s) "15"

let suite =
  "MacroTestList" >::: [
    "test_macro1" >:: test_macro1
  ; "test_macro_dynamic1" >:: test_macro_dynamic1
  ]

let () =
  run_test_tt_main suite