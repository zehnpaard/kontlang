open OUnit2
open Kontlang

let () = Unix.chdir "../../.."

let test_mm1 _ =
  let s = "
  (let [M Stdlib.MaybeMonad]
    (M.reify
      (let [(x (M.reflect (Stdlib.List.find (fn [x] (= x 2)) (list 1 0 3))))
            (y (M.reflect (Stdlib.List.find (fn [x] (= x 3)) (list 1 2 3))))]
        (cons x y))))
  " in
  assert_equal (Execute.eval_string s) "nil"

let test_mm2 _ =
  let s = "
  (let [M Stdlib.MaybeMonad]
    (M.reify
      (let [(x (M.reflect (Stdlib.List.find (fn [x] (= x 2)) (list 1 2 3))))
            (y (M.reflect (Stdlib.List.find (fn [x] (= x 3)) (list 1 2 3))))]
        (cons x y))))
  " in
  assert_equal (Execute.eval_string s) "(2 . 3)"


let suite =
  "StdlibMaybeMonadTestList" >::: [
    "test_mm1" >:: test_mm1
  ; "test_mm2" >:: test_mm2
  ]

let () =
  run_test_tt_main suite