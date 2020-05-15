open OUnit2
open Kontlang

let test_true _ =
  assert_equal (Execute.eval_string "true") "true"

let test_false _ =
  assert_equal (Execute.eval_string "false") "false"

let test_eq0 _ =
  let s = "(=)" in
  assert_raises (Failure "NumBool op = applied to empty list") (fun () -> Execute.eval_string s)

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

let test_neq2 _ =
  let s = "(!= 0 1)" in
  assert_equal (Execute.eval_string s) "true"

let test_neq2_false _ =
  let s = "(!= 2 2)" in
  assert_equal (Execute.eval_string s) "false"

let test_neq_many _ =
  let s = "(!= 1 5 3 (* 3 0) 4)" in
  assert_equal (Execute.eval_string s) "true"

let test_neq_many_false _ =
  let s = "(!= 0 1 2 3 0)" in
  assert_equal (Execute.eval_string s) "false"

let test_and0 _ =
  let s = "(and)" in
  assert_equal (Execute.eval_string s) "true"

let test_and1 _ =
  let s = "(and true)" in
  assert_equal (Execute.eval_string s) "true"

let test_and2 _ =
  let s = "(and (>= 3 1 1) (< 0 3))" in
  assert_equal (Execute.eval_string s) "true"

let test_and_many _ =
  let s = "(and (>= 3 1 1) (< 0 3) true (<= 1 2 3 3 5))" in
  assert_equal (Execute.eval_string s) "true"

let test_and_many_false _ =
  let s = "(and (>= 3 1 1) (< 0 3) false (<= 1 2 3 3 5))" in
  assert_equal (Execute.eval_string s) "false"

let suite =
  "BoolTestList" >::: [
    "test_true" >:: test_true
  ; "test_false" >:: test_false
  ; "test_eq0" >:: test_eq0
  ; "test_eq1" >:: test_eq1
  ; "test_eq2" >:: test_eq2
  ; "test_eq2_false" >:: test_eq2_false
  ; "test_eq_many" >:: test_eq_many
  ; "test_eq_many_false" >:: test_eq_many_false
  ; "test_neq2" >:: test_neq2
  ; "test_neq2_false" >:: test_neq2_false
  ; "test_neq_many" >:: test_neq_many
  ; "test_neq_many_false" >:: test_neq_many_false
  ; "test_and0" >:: test_and0
  ; "test_and1" >:: test_and1
  ; "test_and2" >:: test_and2
  ; "test_and_many" >:: test_and_many
  ; "test_and_many_false" >:: test_and_many_false
  ]

let () =
  run_test_tt_main suite