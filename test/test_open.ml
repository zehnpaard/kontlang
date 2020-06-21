open OUnit2
open Kontlang

let () = Unix.chdir "../../.."

let test_open1 _ =
  let s = "(let [M (module [(define x 10)])]
             (open M x))" in
  assert_equal (Execute.eval_string s) "10"

let test_open2 _ =
  let s = "(let* [(M (module [(define x 10)]))
                  (x 5)]
             (open M x))" in
  assert_equal (Execute.eval_string s) "10"

let test_open3 _ =
  let s = "(let* [(M (module [(define x 10)]))
                  (x 5)]
             (+ x (open M x)))" in
  assert_equal (Execute.eval_string s) "15"

let test_open4 _ =
  let s = "(let* [(M (module [(define x 10)]))
                  (x 5)
                  (y 1)]
             (+ x (open M (+ x y))))" in
  assert_equal (Execute.eval_string s) "16"

let suite =
  "OpenTestList" >::: [
    "test_open1" >:: test_open1
  ; "test_open2" >:: test_open2
  ; "test_open3" >:: test_open3
  ; "test_open4" >:: test_open4
  ]

let () =
  run_test_tt_main suite