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

let test_include3 _ =
  let s = "(let* [(M (module [(define x 10)]))
                  (N (module [(define y (+ x 5))
                              (include M)]))]
             (+ M.x N.y))" in
  assert_raises (Failure "Variable x not found") (fun () -> Execute.eval_string s)

let test_multi_include1 _ =
  let s = "(let* [(M (module [(define x 10)]))
                  (N (module [(define y 5)]))
                  (O (module [(include M) (include N)]))]
             (+ O.x O.y))" in
  assert_equal (Execute.eval_string s) "15"

let test_multi_include2 _ =
  let s = "(let* [(M (module [(define x 10)]))
                  (N (module [(define y 5)]))
                  (O (module [(include M) (include N) (define z (+ x y))]))]
             (+ O.x O.y O.z))" in
  assert_equal (Execute.eval_string s) "30"

let suite =
  "IncludeTestList" >::: [
    "test_include1" >:: test_include1
  ; "test_include2" >:: test_include2
  ; "test_include3" >:: test_include3
  ; "test_multi_include1" >:: test_multi_include1
  ; "test_multi_include2" >:: test_multi_include2
  ]

let () =
  run_test_tt_main suite