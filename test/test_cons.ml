open OUnit2
open Kontlang

let test_cons1 _ =
  let s = "(cons 1 2)" in
  assert_equal (Execute.eval_string s) "(1 . 2)"

let test_car1 _ =
  let s = "(car (cons 1 2))" in
  assert_equal (Execute.eval_string s) "1"

let test_cdr1 _ =
  let s = "(cdr (cons 1 2))" in
  assert_equal (Execute.eval_string s) "2"

let test_car2 _ =
  let s = "(car (cons 1 (cons 2 3)))" in
  assert_equal (Execute.eval_string s) "1"

let test_cdr2 _ =
  let s = "(cdr (cons 1 (cons 2 3)))" in
  assert_equal (Execute.eval_string s) "(2 . 3)"

let test_list1 _ =
  let s = "(list 1 2 3)" in
  assert_equal (Execute.eval_string s) "(1 2 3)"

let test_list2 _ =
  let s = "(cons 0 (list 1 2 3))" in
  assert_equal (Execute.eval_string s) "(0 1 2 3)"

let suite =
  "ConsTestList" >::: [
    "test_cons1" >:: test_cons1
  ; "test_car1" >:: test_car1
  ; "test_cdr1" >:: test_cdr1
  ; "test_car2" >:: test_car2
  ; "test_cdr2" >:: test_cdr2
  ; "test_list1" >:: test_list1
  ; "test_list2" >:: test_list2
  ]

let () =
  run_test_tt_main suite