open OUnit2
open Kontlang

let () = Unix.chdir "../../.."

let test_cr1 _ =
  let s = "
  (let* [(C Stdlib.Coroutine)
         (c (C.coroutine (let [x (C.yield 1)] x)))]
    (car (c)))
  " in
  assert_equal (Execute.eval_string s) "1"

let test_cr2 _ =
  let s = "
  (let* [(C Stdlib.Coroutine)
         (c (C.coroutine (let [x (C.yield 1)] x)))
         (c (cdr (c)))]
    (car (c 2)))
  " in
  assert_equal (Execute.eval_string s) "2"

let test_cr3 _ =
  let s = "
  (let* [(C Stdlib.Coroutine)
         (c (C.coroutine (let [x (C.yield 1)] x)))
         (c (cdr (c)))
         (c (cdr (c 2)))]
    (c 3))
  " in
  assert_equal (Execute.eval_string s) "nil"

let test_multi_cr1 _ =
  let s = "
  (let* [(C Stdlib.Coroutine)
         (c (C.coroutine
              (let [(x (C.yield 1))
                    (y (C.yield 2))]
                (+ x y))))
         (c (cdr (c)))
         (c (cdr (c 3)))]
    (car (c 4)))
  " in
  assert_equal (Execute.eval_string s) "7"

let test_multi_cr2 _ =
  let s = "
  (let* [(C Stdlib.Coroutine)
         (c (C.coroutine
              (let [(x (C.yield 1))
                    (y (C.yield 2))]
                (+ x y))))
         (c (cdr (c)))
         (c (cdr (c 3)))
         (c (cdr (c 4)))]
    (c 5))
  " in
  assert_equal (Execute.eval_string s) "nil"

let suite =
  "CoroutineTestList" >::: [
    "test_cr1" >:: test_cr1
  ; "test_cr2" >:: test_cr2
  ; "test_cr3" >:: test_cr3
  ; "test_multi_cr1" >:: test_multi_cr1
  ; "test_multi_cr2" >:: test_multi_cr2
  ]

let () =
  run_test_tt_main suite