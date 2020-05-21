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

let test_let_fn1 _ =
  let s = "(let [x 3] ((fn [x] (+ x x)) x))" in
  assert_equal (Execute.eval_string s) "6"

let test_let_fn2 _ =
  let s = "(let [x 3] (+ ((fn [x] (+ x x)) 5) x))" in
  assert_equal (Execute.eval_string s) "13"

let test_fn_let1 _ =
  let s = "((fn [x] (+ x (let [x 3] x))) 5)" in
  assert_equal (Execute.eval_string s) "8"

let test_fn_let2 _ =
  let s = "((fn [x] (+ (let [x 3] x) x)) 5)" in
  assert_equal (Execute.eval_string s) "8"

let test_letfn1 _ =
  let s = "(letfn [f [x] (+ x x)] (f 5))" in
  assert_equal (Execute.eval_string s) "10"

let test_letfn2 _ =
  let s = "(let [(x 3) (f 1)] (+ (letfn [f [x] (+ x x)] (f 5)) x f))" in
  assert_equal (Execute.eval_string s) "14"

let test_letfn_multi1 _ =
  let s = "(letfn [(f [x] (+ x x)) (g [y] (* y y))] (- (g 10) (f 5)))" in
  assert_equal (Execute.eval_string s) "90"

let suite =
  "FnTestList" >::: [
    "test_fn1" >:: test_fn1
  ; "test_fn_error1" >:: test_fn_error1
  ; "test_let_fn1" >:: test_let_fn1
  ; "test_let_fn2" >:: test_let_fn2
  ; "test_fn_let1" >:: test_fn_let1 
  ; "test_fn_let2" >:: test_fn_let2 
  ; "test_letfn1" >:: test_letfn1
  ; "test_letfn2" >:: test_letfn2
  ; "test_letfn_multi1" >:: test_letfn_multi1
  ]

let () =
  run_test_tt_main suite