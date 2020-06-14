open OUnit2
open Kontlang

let () = Unix.chdir "../../.."

let test_string1 _ =
  let s = "\"hello world\"" in
  assert_equal (Execute.eval_string s) "\"hello world\""

let test_concat1 _ =
  let s = "(concat \"a\" \"b\")" in
  assert_equal (Execute.eval_string s) "\"ab\""

let test_concat2 _ =
  let s = "(let [x \"c\"] (concat \"a\" \"b\" x))" in
  assert_equal (Execute.eval_string s) "\"abc\""

let test_do1 _ =
  let s = "(do [1])" in
  assert_equal (Execute.eval_string s) "1"

let test_do2 _ =
  let s = "(do [1 2 3])" in
  assert_equal (Execute.eval_string s) "3"

let suite =
  "SideEffectTestList" >::: [
    "test_string1" >:: test_string1
  ; "test_concat1" >:: test_concat1
  ; "test_concat2" >:: test_concat2
  ; "test_do1" >:: test_do1
  ; "test_do2" >:: test_do2
  ]

let () =
  run_test_tt_main suite