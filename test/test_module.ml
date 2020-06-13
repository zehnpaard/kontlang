open OUnit2
open Kontlang

let test_module1 _ =
  let s = "(let [M (module [(define x 10)])] M.x)" in
  assert_equal (Execute.eval_string s) "10"

let test_module2 _ =
  let s = "(let [M (module [(+ 1 2) (define x 10)])] M.x)" in
  assert_equal (Execute.eval_string s) "10"

let test_nested_module1 _ =
  let s =
  "(let [M (module
             [(define x 10)
              (define Y (module [(define z 5)]))])]
     (+ M.x M.Y.z))"
  in
  assert_equal (Execute.eval_string s) "15"

let suite =
  "MacroTestList" >::: [
    "test_module1" >:: test_module1
  ; "test_module2" >:: test_module2
  ; "test_nested_module1" >:: test_nested_module1
  ]

let () =
  run_test_tt_main suite