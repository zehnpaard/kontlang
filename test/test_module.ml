open OUnit2
open Kontlang

let () = Unix.chdir "../../.."

let test_module1 _ =
  let s = "(let [M (module [(define x 10)])] M.x)" in
  assert_equal (Execute.eval_string s) "10"

let test_module2 _ =
  let s = "(let [M (module [(+ 1 2) (define x 10)])] M.x)" in
  assert_equal (Execute.eval_string s) "10"

let test_nested_module1 _ =
  let s =
  "(let [M (module
             [(define x 10)
              (define Y (module [(define z 5)]))])]
     (+ M.x M.Y.z))"
  in
  assert_equal (Execute.eval_string s) "15"

let test_functor1 _ =
  let s = "
  (let* [(X (module [(define x 1)]))
         (F (fn [M] (module [(define y M.x)])))
         (Y (F X))]
    Y.y)
  " in
  assert_equal (Execute.eval_string s) "1"

let test_functor2 _ =
  let s = "
  (let* [(X (module [(define x 1)]))
         (F (fn [M] (module [(define M M)])))
         (Y (F X))]
    Y.M.x)
  " in
  assert_equal (Execute.eval_string s) "1"

let suite =
  "MacroTestList" >::: [
    "test_module1" >:: test_module1
  ; "test_module2" >:: test_module2
  ; "test_nested_module1" >:: test_nested_module1
  ; "test_functor1" >:: test_functor1
  ; "test_functor2" >:: test_functor2
  ]

let () =
  run_test_tt_main suite