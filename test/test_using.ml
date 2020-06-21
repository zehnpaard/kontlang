open OUnit2
open Kontlang

let () = Unix.chdir "../../.."

let test_using1 _ =
  let s = "(let* [(M (module [(define x 10)]))
                  (N (module [(using M)]))]
             (+ M.x N.x))" in
  assert_raises (Failure "Member x of module does not exist") (fun () -> Execute.eval_string s)

let test_using2 _ =
  let s = "(let* [(M (module [(define x 10)]))
                  (N (module [(using M)
                              (define y (+ x 5))]))]
             (+ M.x N.y))" in
  assert_equal (Execute.eval_string s) "25"

let test_using3 _ =
  let s = "(let* [(M (module [(define x 10)]))
                  (N (module [(using M)
                              (define y (+ x 5))]))]
             (+ M.x N.x N.y))" in
  assert_raises (Failure "Member x of module does not exist") (fun () -> Execute.eval_string s)

let test_using4 _ =
  let s = "(let* [(M (module [(define x 10)]))
                  (N (module [(define y (+ x 5))
                              (using M)]))]
             (+ M.x N.y))" in
  assert_raises (Failure "Variable x not found") (fun () -> Execute.eval_string s)

let test_multi_using1 _ =
  let s = "(let* [(M (module [(define x 10)]))
                  (N (module [(define y 5)]))
                  (O (module [(using M) (using N) (define z (+ x y))]))]
             O.z)" in
  assert_equal (Execute.eval_string s) "15"

let suite =
  "UsingTestList" >::: [
    "test_using1" >:: test_using1
  ; "test_using2" >:: test_using2
  ; "test_using3" >:: test_using3
  ; "test_using4" >:: test_using4
  ; "test_multi_using1" >:: test_multi_using1
  ]

let () =
  run_test_tt_main suite