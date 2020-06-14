open OUnit2
open Kontlang

let () = Unix.chdir "../../.."

let test_letrec1 _ =
  let s = "(letrec [f [x y] (if (= y 0) 0 (+ x (f x (- y 1))))] (f 5 5))" in
  assert_equal (Execute.eval_string s) "25"

let test_letrec2 _ =
  let s = "(letrec [f [x] (if (= x 1) 1 (* x (f (- x 1))))] (f 5))" in
  assert_equal (Execute.eval_string s) "120"

let test_letrec_multi1 _ =
  let s = "
  (letrec [(odd? [x] (if (= x 1)
                         true
                         (not (even? (- x 1)))))
           (even? [x] (if (= x 0)
                          true
                          (not (odd? (- x 1)))))]
    (odd? 11))
  " in
  assert_equal (Execute.eval_string s) "true"

let test_letrec_multi2 _ =
  let s = "
  (letrec [(odd? [x] (if (= x 0)
                         false
                         (even? (- x 1))))
           (even? [x] (if (= x 0)
                          true
                          (odd? (- x 1))))]
    (even? 23))
  " in
  assert_equal (Execute.eval_string s) "false"

let test_fn_letrec1 _ =
  let s = "
  ((fn [x]
     (letrec [(f [x] (g x))
              (g [x] (+ x 1))]
       (f (g x))))
    5)
  " in
  assert_equal (Execute.eval_string s) "7"

let suite =
  "LetRecTestList" >::: [
    "test_letrec1" >:: test_letrec1
  ; "test_letrec2" >:: test_letrec2
  ; "test_letrec_multi1" >:: test_letrec_multi1
  ; "test_letrec_multi2" >:: test_letrec_multi2
  ; "test_fn_letrec1" >:: test_fn_letrec1
  ]

let () =
  run_test_tt_main suite