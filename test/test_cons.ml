open OUnit2
open Kontlang

let () = Unix.chdir "../../.."

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

let test_apply _ =
  let s = "(apply * (list 1 2 3 4 5))" in
  assert_equal (Execute.eval_string s) "120"

let test_nilp1 _ =
  let s = "(nil? (cdr (list 1)))" in
  assert_equal (Execute.eval_string s) "true"

let test_nilp2 _ =
  let s = "(nil? (car (list nil 1)))" in
  assert_equal (Execute.eval_string s) "true"

let test_nilp3 _ =
  let s = "(nil? 1)" in
  assert_equal (Execute.eval_string s) "false"

let test_nilp4 _ =
  let s = "(nil? (list 1))" in
  assert_equal (Execute.eval_string s) "false"

let test_consp1 _ =
  let s = "(cons? (list 1))" in
  assert_equal (Execute.eval_string s) "true"

let test_consp2 _ =
  let s = "(cons? (cons nil 1))" in
  assert_equal (Execute.eval_string s) "true"

let test_consp3 _ =
  let s = "(cons? 1)" in
  assert_equal (Execute.eval_string s) "false"

let test_consp4 _ =
  let s = "(cons? nil)" in
  assert_equal (Execute.eval_string s) "false"

let suite =
  "ConsTestList" >::: [
    "test_cons1" >:: test_cons1
  ; "test_car1" >:: test_car1
  ; "test_cdr1" >:: test_cdr1
  ; "test_car2" >:: test_car2
  ; "test_cdr2" >:: test_cdr2
  ; "test_list1" >:: test_list1
  ; "test_list2" >:: test_list2
  ; "test_apply" >:: test_apply
  ; "test_nilp1" >:: test_nilp1
  ; "test_nilp2" >:: test_nilp2
  ; "test_nilp3" >:: test_nilp3
  ; "test_nilp4" >:: test_nilp4
  ; "test_consp1" >:: test_consp1
  ; "test_consp2" >:: test_consp2
  ; "test_consp3" >:: test_consp3
  ; "test_consp4" >:: test_consp4
  ]

let () =
  run_test_tt_main suite