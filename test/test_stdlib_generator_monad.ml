open OUnit2
open Kontlang

let () = Unix.chdir "../../.."

let test_simple1 _ =
  let s = "
  (let* [(G Stdlib.Generator)
         (GM Stdlib.GeneratorMonad)
         (g (GM.reify
              (let [(x (GM.reflect (fn [] (cons 1 (fn [] nil)))))
                    (y (GM.reflect (fn [] (cons 2 (fn [] nil)))))]
                (cons x y))))]
    (car (g)))
  " in
  assert_equal (Execute.eval_string s) "(1 . 2)"

let test_simple2 _ =
  let s = "
  (let* [(G Stdlib.Generator)
         (GM Stdlib.GeneratorMonad)
         (g (GM.reify
              (let [(x (GM.reflect (fn [] (cons 1 (fn [] nil)))))
                    (y (GM.reflect (fn [] (cons 2 (fn [] nil)))))
                    (z (GM.reflect (fn [] nil)))]
                (cons x y))))]
    (g))
  " in
  assert_equal (Execute.eval_string s) "nil"

let test_from_list1 _ =
  let s = "
  (let* [(G Stdlib.Generator)
         (GM Stdlib.GeneratorMonad)
         (g (GM.reify
              (let [(x (GM.reflect (G.from_list (list 1 2))))
                    (y (GM.reflect (G.from_list (list 3 4))))]
                (cons x y))))]
    (car (g)))
  " in
  assert_equal (Execute.eval_string s) "(1 . 3)"

let test_from_list2 _ =
  let s = "
  (let* [(G Stdlib.Generator)
         (GM Stdlib.GeneratorMonad)
         (g (GM.reify
              (let [(x (GM.reflect (G.from_list (list 1 2))))
                    (y (GM.reflect (G.from_list (list 3 4))))]
                (cons x y))))]
    (G.to_list g))
  " in
  assert_equal (Execute.eval_string s) "((1 . 3) (1 . 4) (2 . 3) (2 . 4))"

let test_from_list_assert1 _ =
  let s = "
  (let* [(G Stdlib.Generator)
         (GM Stdlib.GeneratorMonad)
         (g (GM.reify
              (let* [(x (GM.reflect (G.from_list (list 1 3))))
                     (y (GM.reflect (G.from_list (list 2 4))))
                     (_ (GM.assert (> x y)))]
                (cons x y))))]
    (car (g)))
  " in
  assert_equal (Execute.eval_string s) "(3 . 2)"

let test_from_list_assert2 _ =
  let s = "
  (let* [(G Stdlib.Generator)
         (GM Stdlib.GeneratorMonad)
         (g (GM.reify
              (let* [(x (GM.reflect (G.from_list (list 1 3))))
                     (y (GM.reflect (G.from_list (list 2 4))))
                     (_ (GM.assert (> x y)))]
                (cons x y))))]
    (G.to_list g))
  " in
  assert_equal (Execute.eval_string s) "((3 . 2))"

let test_gm1 _ =
  let s = "
  (let* [(G Stdlib.Generator)
         (GM Stdlib.GeneratorMonad)
         (g (GM.reify
           (let [(x (GM.reflect (G.take 5 (G.countn 1))))
                 (y (GM.reflect (G.take 5 (G.countn 1))))
                 (z (GM.reflect (G.take 5 (G.countn 1))))]
             (if (= (* z z)
                    (+ (* x x)
                       (* y y)))
               (list x y z)
               (GM.fail)))))]
    (car (g)))
  " in
  assert_equal (Execute.eval_string s) "(3 4 5)"

let test_gm2 _ =
  let s = "
  (let* [(G Stdlib.Generator)
         (GM Stdlib.GeneratorMonad)
         (g (GM.reify
           (let* [(x (GM.reflect (G.take 5 (G.countn 1))))
                  (y (GM.reflect (G.take 5 (G.countn 1))))
                  (z (GM.reflect (G.take 5 (G.countn 1))))
                  (_ (GM.assert (= (* z z)
                                   (+ (* x x) (* y y)))))]
             (list x y z))))]
    (G.head g))
  " in
  assert_equal (Execute.eval_string s) "(3 4 5)"

let test_gm3 _ =
  let s = "
  (let* [(G Stdlib.Generator)
         (GM Stdlib.GeneratorMonad)
         (g (GM.reify
           (let* [(x (GM.reflect (G.take 5 (G.countn 1))))
                  (y (GM.reflect (G.take 5 (G.countn 1))))
                  (z (GM.reflect (G.take 5 (G.countn 1))))
                  (_ (GM.assert (= (* z z)
                                   (+ (* x x) (* y y)))))]
             (list x y z))))]
    (G.head (G.tail g)))
  " in
  assert_equal (Execute.eval_string s) "(4 3 5)"

let test_gm4 _ =
  let s = "
  (let* [(G Stdlib.Generator)
         (GM Stdlib.GeneratorMonad)
         (g (GM.reify
           (let* [(x (GM.reflect (G.take 5 (G.countn 1))))
                  (y (GM.reflect (G.take 5 (G.countn 1))))
                  (z (GM.reflect (G.take 5 (G.countn 1))))
                  (_ (GM.assert (= (* z z)
                                   (+ (* x x) (* y y)))))]
             (list x y z))))]
    (G.to_list g))
  " in
  assert_equal (Execute.eval_string s) "((3 4 5) (4 3 5))"

let suite =
  "StdlibGeneratorMonadTestList" >::: [
    "test_simple1" >:: test_simple1
  ; "test_simple2" >:: test_simple2
  ; "test_from_list1" >:: test_from_list1
  ; "test_from_list2" >:: test_from_list2
  ; "test_from_list_assert1" >:: test_from_list_assert1
  ; "test_from_list_assert2" >:: test_from_list_assert2
  ; "test_gm1" >:: test_gm1
  ; "test_gm2" >:: test_gm2
  ; "test_gm3" >:: test_gm3
  ; "test_gm4" >:: test_gm4
  ]

let () =
  run_test_tt_main suite