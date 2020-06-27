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

let test_rec_cr1 _ =
  let s = "
  (let [(C Stdlib.Coroutine)]
    (letrec [f [x] (f (+ x (C.yield x)))]
      (let* [(c (C.coroutine (f 0)))
             (c (cdr (c)))
             (c (cdr (c 1)))
             (c (cdr (c 2)))]
        (car (c 3)))))
  " in
  assert_equal (Execute.eval_string s) "6"

let test_cr_helper1 _ =
  let s = "
  (let* [(C Stdlib.Coroutine)
         (c (C.coroutine (let [x (C.yield 1)] x)))]
    (C.val (C.start c)))
  " in
  assert_equal (Execute.eval_string s) "1"

let test_cr_helper2 _ =
  let s = "
  (let* [(C Stdlib.Coroutine)
         (c (C.coroutine (let [x (C.yield 1)] x)))
         (c (C.start c))]
    (C.val (C.send c 2)))
  " in
  assert_equal (Execute.eval_string s) "2"

let test_cr_helper3 _ =
  let s = "
  (let* [(C Stdlib.Coroutine)
         (c (C.coroutine (let [x (C.yield 1)] x)))
         (c (C.start c))
         (c (C.send c 2))]
    (C.send c 3))
  " in
  assert_equal (Execute.eval_string s) "nil"

let test_multi_cr_helper1 _ =
  let s = "
  (let* [(C Stdlib.Coroutine)
         (c (C.coroutine
              (let [(x (C.yield 1))
                    (y (C.yield 2))]
                (+ x y))))
         (c (C.start c))
         (c (C.send c 3))]
    (C.val (C.send c 4)))
  " in
  assert_equal (Execute.eval_string s) "7"

let test_multi_cr_helper2 _ =
  let s = "
  (let* [(C Stdlib.Coroutine)
         (c (C.coroutine
              (let [(x (C.yield 1))
                    (y (C.yield 2))]
                (+ x y))))
         (c (C.start c))
         (c (C.send c 3))
         (c (C.send c 4))]
    (C.send c 5))
  " in
  assert_equal (Execute.eval_string s) "nil"

let test_rec_cr_helper1 _ =
  let s = "
  (let [(C Stdlib.Coroutine)]
    (letrec [f [x] (f (+ x (C.yield x)))]
      (let* [(c (C.coroutine (f 0)))
             (c (C.start c))
             (c (C.send c 1))
             (c (C.send c 2))]
        (C.val (C.send c 3)))))
  " in
  assert_equal (Execute.eval_string s) "6"

let suite =
  "CoroutineTestList" >::: [
    "test_cr1" >:: test_cr1
  ; "test_cr2" >:: test_cr2
  ; "test_cr3" >:: test_cr3
  ; "test_multi_cr1" >:: test_multi_cr1
  ; "test_multi_cr2" >:: test_multi_cr2
  ; "test_rec_cr1" >:: test_rec_cr1
  ; "test_cr1" >:: test_cr_helper1
  ; "test_cr2" >:: test_cr_helper2
  ; "test_cr3" >:: test_cr_helper3
  ; "test_multi_cr1" >:: test_multi_cr_helper1
  ; "test_multi_cr2" >:: test_multi_cr_helper2
  ; "test_rec_cr1" >:: test_rec_cr_helper1
  ]

let () =
  run_test_tt_main suite