open OUnit2
open Kontlang

let test_fn1 _ =
  let s = "((fn [x] (+ x x)) 5)" in
  assert_equal (Execute.eval_string s) "10"

let test_fn_error1 _ =
  let s = "((fn [x] (+ x x)) 5 5)" in
  let thunk = (fun () -> Execute.eval_string s) in
  let msg = "Function anon called with incorrect number of args: expected 1 received 2" in
  assert_raises (Failure msg) thunk

let suite =
  "FnTestList" >::: [
    "test_fn1" >:: test_fn1
  ; "test_fn_error1" >:: test_fn_error1
  ]

let () =
  run_test_tt_main suite