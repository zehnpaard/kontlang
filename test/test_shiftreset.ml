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

let suite =
  "ShiftResetTestList" >::: [
    "test_reset1" >:: test_reset1
  ; "test_sr1" >:: test_sr1
  ; "test_sr2" >:: test_sr2
  ; "test_sr3" >:: test_sr3
  ; "test_sr4" >:: test_sr4
  ]

let () =
  run_test_tt_main suite