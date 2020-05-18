open OUnit2
open Kontlang

let test_let1 _ =
  let s = "(let [x 10] x)" in
  assert_equal (Execute.eval_string s) "10"

let test_let2 _ =
  let s = "(let [x 10] 5)" in
  assert_equal (Execute.eval_string s) "5"

let test_let3 _ =
  let s = "(let [x 10] (+ x 5))" in
  assert_equal (Execute.eval_string s) "15"

let test_let4 _ =
  let s = "(let [x 10] (let [y 5] (+ x y)))" in
  assert_equal (Execute.eval_string s) "15"

let suite =
  "LetTestList" >::: [
    "test_let1" >:: test_let1
  ; "test_let2" >:: test_let2
  ; "test_let3" >:: test_let3
  ; "test_let4" >:: test_let4
  ]

let () =
  run_test_tt_main suite