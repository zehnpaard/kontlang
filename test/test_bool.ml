open OUnit2
open Kontlang

let test_true _ =
  assert_equal (Execute.eval_string "true") "true"

let test_false _ =
  assert_equal (Execute.eval_string "false") "false"

let test_eq1 _ =
  let s = "(= 0)" in
  assert_raises (Failure "NumBool op = applied to one arg") (fun () -> Execute.eval_string s)

let test_eq2 _ =
  let s = "(= 0 0)" in
  assert_equal (Execute.eval_string s) "true"

let test_eq2_false _ =
  let s = "(= 2 3)" in
  assert_equal (Execute.eval_string s) "false"

let test_eq_many _ =
  let s = "(= 0 0 0 (* 3 0) 0)" in
  assert_equal (Execute.eval_string s) "true"

let test_eq_many_false _ =
  let s = "(= 0 0 0 3 0)" in
  assert_equal (Execute.eval_string s) "false"

let suite =
  "BoolTestList" >::: [
    "test_true" >:: test_true
  ; "test_false" >:: test_false
  ; "test_eq1" >:: test_eq1
  ; "test_eq2" >:: test_eq2
  ; "test_eq2_false" >:: test_eq2_false
  ; "test_eq_many" >:: test_eq_many
  ; "test_eq_many_false" >:: test_eq_many_false
  ]

let () =
  run_test_tt_main suite