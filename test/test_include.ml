open OUnit2
open Kontlang

let () = Unix.chdir "../../.."

let test_include1 _ =
  let s = "(let* [(M (module [(define x 10)]))
                  (N (module [(include M)]))]
             (+ M.x N.x))" in
  assert_equal (Execute.eval_string s) "20"

let test_include2 _ =
  let s = "(let* [(M (module [(define x 10)]))
                  (N (module [(include M)
                              (define y (+ x 5))]))]
             (+ M.x N.y))" in
  assert_equal (Execute.eval_string s) "25"

let suite =
  "IncludeTestList" >::: [
    "test_include1" >:: test_include1
  ; "test_include2" >:: test_include2
  ]

let () =
  run_test_tt_main suite