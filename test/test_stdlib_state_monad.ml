open OUnit2
open Kontlang

let () = Unix.chdir "../../.."

let test_mm1 _ =
  let s = "
  (let [S Stdlib.StateMonad]
    (S.run_state (S.reify 5) 0))
  " in
  assert_equal (Execute.eval_string s) "(5 . 0)"

let test_mm2 _ =
  let s = "
  (let [S Stdlib.StateMonad]
    (S.run_state (S.reify (S.get)) 0))
  " in
  assert_equal (Execute.eval_string s) "(0 . 0)"

let test_mm3 _ =
  let s = "
  (let [S Stdlib.StateMonad]
    (S.run_state (S.reify (S.put 1)) 0))
  " in
  assert_equal (Execute.eval_string s) "(nil . 1)"

let test_put_get _ =
  let s = "
  (let [S Stdlib.StateMonad]
    (S.run_state (S.reify (do [(S.put 1) (S.get)])) 0))
  " in
  assert_equal (Execute.eval_string s) "(1 . 1)"

let test_get_put _ =
  let s = "
  (let [S Stdlib.StateMonad]
    (S.run_state (S.reify (do [(S.get) (S.put 1)])) 0))
  " in
  assert_equal (Execute.eval_string s) "(nil . 1)"

let test_tick _ =
  let s = "
  (let [S Stdlib.StateMonad]
    (S.run_state
      (S.reify
        (let* [(tick (fn [] (S.put (+ (S.get) 1))))
               (_ (tick))
               (_ (tick))
               (a (S.get))
               (_ (tick))]
           (- (S.get) a)))
      0))
  " in
  assert_equal (Execute.eval_string s) "(1 . 3)"

let test_tick2 _ =
  let s = "
  (let [S Stdlib.StateMonad]
    (S.run_state
      (S.reify
        (let* [(tick
                 (fn []
                   (S.reflect
                     (fn [state] (cons nil (+ state 1))))))
               (_ (tick))
               (_ (tick))
               (a (S.get))
               (_ (tick))]
           (- (S.get) a)))
      0))
  " in
  assert_equal (Execute.eval_string s) "(1 . 3)"

let suite =
  "StdlibStateMonadTestList" >::: [
    "test_mm1" >:: test_mm1
  ; "test_mm2" >:: test_mm2
  ; "test_mm3" >:: test_mm3
  ; "test_put_get" >:: test_put_get
  ; "test_get_put" >:: test_get_put
  ; "test_tick" >:: test_tick
  ; "test_tick2" >:: test_tick2
  ]

let () =
  run_test_tt_main suite