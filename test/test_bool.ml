open OUnit2
open Kontlang

let test_true _ =
  assert_equal (Execute.eval_string "true") "true"

let test_false _ =
  assert_equal (Execute.eval_string "false") "false"

let suite =
  "BoolTestList" >::: [
    "test_true" >:: test_true
  ; "test_false" >:: test_false
  ]

let () =
  run_test_tt_main suite