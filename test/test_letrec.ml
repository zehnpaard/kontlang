open OUnit2
open Kontlang

let test_letrec1 _ =
  let s = "(letrec [(f [x y] (if (= y 0) 0 (+ x (f x (- y 1)))))] (f 5 5))" in
  assert_equal (Execute.eval_string s) "25"

let test_letrec2 _ =
  let s = "(letrec [(f [x] (if (= x 1) 1 (* x (f (- x 1)))))] (f 5))" in
  assert_equal (Execute.eval_string s) "120"

let suite =
  "LetRecTestList" >::: [
    "test_letrec1" >:: test_letrec1
  ; "test_letrec2" >:: test_letrec2
  ]

let () =
  run_test_tt_main suite