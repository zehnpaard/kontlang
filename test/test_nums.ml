open OUnit2
open Kontlang

let test_sum1 _ =
  let s = "(+ 1)" in
  assert_equal (Execute.eval_string s) "1"

let test_sum_many _ =
  let s = "(+ 1 2 3 4 5)" in
  assert_equal (Execute.eval_string s) "15"

let test_sub_many _ =
  let s = "(- 15 1 2 3 4 5)" in
  assert_equal (Execute.eval_string s) "0"

let test_many _ =
  let s = "(- (* 5 3) 1 2 3 (+ 2 2) 5)" in
  assert_equal (Execute.eval_string s) "0"

let suite =
  "NumTestList" >::: [
    "test_sum1" >:: test_sum1
  ; "test_sum_many" >:: test_sum_many
  ; "test_sub_many" >:: test_sub_many
  ; "test_many" >:: test_many
  ]

let () =
  run_test_tt_main suite