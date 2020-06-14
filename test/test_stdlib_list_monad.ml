open OUnit2
open Kontlang

let () = Unix.chdir "../../.."

let test_lm1 _ =
  let s = "
  (let [L Stdlib.ListMonad]
    (L.reify
      (let [(x (L.reflect (list 1 2)))
            (y (L.reflect (list 3 4)))]
        (L.return (cons x y)))))
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
          (L.return (list x y z))
          nil))))
  " in
  assert_equal (Execute.eval_string s) "((3 4 5) (4 3 5))"


let suite =
  "StdlibListMonadTestList" >::: [
    "test_lm1" >:: test_lm1
  ; "test_lm2" >:: test_lm2
  ]

let () =
  run_test_tt_main suite