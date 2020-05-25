open OUnit2
open Kontlang

let test_cond1 _ =
  let s = "(cond [true 1])" in
  assert_equal (Execute.eval_string s) "1"

let test_cond2 _ =
  let s = "(cond [true 1] [true 2])" in
  assert_equal (Execute.eval_string s) "1"

let test_cond3 _ =
  let s = "(cond [false 1] [true 2])" in
  assert_equal (Execute.eval_string s) "2"

let test_cond_fail1 _ =
  let s = "(cond [false 1] [false 2])" in
  assert_raises (Failure "Evaluating cond with no matching condition") (fun () -> Execute.eval_string s)

let test_cond_fail2 _ =
  let s = "(cond [1 1] [true 2])" in
  assert_raises (Failure "Non-boolean in condition position of Cond expression") (fun () -> Execute.eval_string s)

let test_map1 _ =
  let s = "
  (letrec [map [f xs]
    (cond
      [(nil? xs) nil]
      [(cons? xs) (cons (f (car xs)) (map f (cdr xs)))]
      [true (f xs)]
    )]
    (map (fn [x] (* 2 x)) (list 1 2 3 4 5)))
  " in
  assert_equal (Execute.eval_string s) "(2 4 6 8 10)"

let test_map2 _ =
  let s = "
  (letrec [map [f xs]
    (cond
      [(nil? xs) nil]
      [(cons? xs) (cons (f (car xs)) (map f (cdr xs)))]
      [true (f xs)]
    )]
    (map (fn [x] (* 2 x)) (cons 1 (cons 2 (cons 3 (cons 4 5))))))
  " in
  assert_equal (Execute.eval_string s) "(2 4 6 8 . 10)"

let suite =
  "CondTestList" >::: [
    "test_cond1" >:: test_cond1
  ; "test_cond2" >:: test_cond2
  ; "test_cond3" >:: test_cond3
  ; "test_cond_fail1" >:: test_cond_fail1
  ; "test_cond_fail2" >:: test_cond_fail2
  ; "test_map1" >:: test_map1
  ; "test_map2" >:: test_map2
  ]

let () =
  run_test_tt_main suite