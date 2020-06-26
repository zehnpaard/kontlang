open OUnit2
open Kontlang

let () = Unix.chdir "../../.."

let test_reset1 _ =
  let s = "(reset 1)" in
  assert_equal (Execute.eval_string s) "1"

let test_sr1 _ =
  let s = "
  (reset
    (shift [k] 1))
  " in
  assert_equal (Execute.eval_string s) "1"

let test_sr2 _ =
  let s = "
  (reset
    (+ 10
      (shift [k] 1)))
  " in
  assert_equal (Execute.eval_string s) "1"

let test_sr3 _ =
  let s = "
  (reset
    (+ 10
      (shift [k] (k 1))))
  " in
  assert_equal (Execute.eval_string s) "11"

let test_sr4 _ =
  let s = "
  ((reset
     (+ 10
       (shift [k] k)))
   1)
  " in
  assert_equal (Execute.eval_string s) "11"

let test_sr_fn1 _ =
  let s = "
  (letfn [f [] (shift [k] 1)]
    (reset (f)))
  " in
  assert_equal (Execute.eval_string s) "1"

let test_sr_fn2 _ =
  let s = "
  (letfn [f [] (shift [k] 1)]
    (reset
      (+ 10 (f))))
  " in
  assert_equal (Execute.eval_string s) "1"

let test_sr_fn3 _ =
  let s = "
  (letfn [f [] (shift [k] (k 1))]
    (reset
      (+ 10 (f))))
  " in
  assert_equal (Execute.eval_string s) "11"

let test_sr_fn4 _ =
  let s = "
  (letfn [f [] (shift [k] k)]
    ((reset
       (+ 10 (f)))
     1))
  " in
  assert_equal (Execute.eval_string s) "11"


let test_multi_shift1 _ =
  let s = "
  (let [f (reset
            (+
              (* 100 (shift [k] k))
              (* 10 (shift [k] k))
              (shift [k] k)))]
   (((f 1) 2) 3))
  " in
  assert_equal (Execute.eval_string s) "123"

let test_acc1 _ =
  let s = "
  (letrec [(f [n]
             (f (+ (shift [k] (cons n k))
                   n)))
           (accumulator []
             (reset (f 0)))]
    (let* [(pair (accumulator))
           (a (car pair))
           (k (cdr pair))
           (pair (k 5))
           (b (car pair))
           (k (cdr pair))
           (pair (k 10))
           (c (car pair))]
       (list a b c)))
  " in
  assert_equal (Execute.eval_string s) "(0 5 15)"

let suite =
  "ShiftResetTestList" >::: [
    "test_reset1" >:: test_reset1
  ; "test_sr1" >:: test_sr1
  ; "test_sr2" >:: test_sr2
  ; "test_sr3" >:: test_sr3
  ; "test_sr4" >:: test_sr4
  ; "test_sr_fn1" >:: test_sr_fn1
  ; "test_sr_fn2" >:: test_sr_fn2
  ; "test_sr_fn3" >:: test_sr_fn3
  ; "test_sr_fn4" >:: test_sr_fn4
  ; "test_multi_shift1" >:: test_multi_shift1
  ; "test_acc1" >:: test_acc1
  ]

let () =
  run_test_tt_main suite