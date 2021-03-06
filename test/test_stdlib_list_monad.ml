open OUnit2
open Kontlang

let () = Unix.chdir "../../.."

let test_lm1 _ =
  let s = "
  (let [L Stdlib.ListMonad]
    (L.reify
      (let [(x (L.reflect (list 1 2)))
            (y (L.reflect (list 3 4)))]
        (cons x y))))
  " in
  assert_equal (Execute.eval_string s) "((1 . 3) (1 . 4) (2 . 3) (2 . 4))"

let test_lm2 _ =
  let s = "
  (let [L Stdlib.ListMonad]
    (L.reify
      (let [(x (L.reflect (list 1 2 3 4 5)))
            (y (L.reflect (list 1 2 3 4 5)))
            (z (L.reflect (list 1 2 3 4 5)))]
        (if (= (* z z)
               (+ (* x x)
                  (* y y)))
          (list x y z)
          (L.fail)))))
  " in
  assert_equal (Execute.eval_string s) "((3 4 5) (4 3 5))"

let test_lm3 _ =
  let s = "
  (let [L Stdlib.ListMonad]
    (L.reify
      (let* [(x (L.reflect (list 1 2 3 4 5)))
             (y (L.reflect (list 1 2 3 4 5)))
             (z (L.reflect (list 1 2 3 4 5)))
             (_ (L.assert  (= (* z z)
                              (+ (* x x) (* y y)))))]
        (list x y z))))
  " in
  assert_equal (Execute.eval_string s) "((3 4 5) (4 3 5))"

let suite =
  "StdlibListMonadTestList" >::: [
    "test_lm1" >:: test_lm1
  ; "test_lm2" >:: test_lm2
  ; "test_lm3" >:: test_lm3
  ]

let () =
  run_test_tt_main suite