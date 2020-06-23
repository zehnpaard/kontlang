open OUnit2
open Kontlang

let () = Unix.chdir "../../.."

let test_gen1 _ =
  let s = "
  (let* [(G Stdlib.Generator)
         (g (G.generator (do [(G.yield 1) 2])))]
    (car (g)))
  " in
  assert_equal (Execute.eval_string s) "1"

let test_gen2 _ =
  let s = "
  (let* [(G Stdlib.Generator)
         (g (G.generator (do [(G.yield 1) 2])))
         (g (cdr (g)))]
    (car (g)))
  " in
  assert_equal (Execute.eval_string s) "2"

let test_gen3 _ =
  let s = "
  (let* [(G Stdlib.Generator)
         (g (G.generator (do [(G.yield 1) 2])))
         (g (cdr (g)))]
    (cdr (g)))
  " in
  assert_equal (Execute.eval_string s) "nil"

let test_from_list1 _ =
  let s = "
  (let* [(G Stdlib.Generator)
         (g (G.from_list (list 1 2)))]
    (car (g)))
  " in
  assert_equal (Execute.eval_string s) "1"

let test_from_list2 _ =
  let s = "
  (let* [(G Stdlib.Generator)
         (g (G.from_list (list 1 2)))
         (g (cdr (g)))]
    (car (g)))
  " in
  assert_equal (Execute.eval_string s) "2"

let test_from_list3 _ =
  let s = "
  (let* [(G Stdlib.Generator)
         (g (G.from_list (list 1 2)))
         (g (cdr (g)))]
    (cdr (g)))
  " in
  assert_equal (Execute.eval_string s) "nil"

let suite =
  "GeneratorTestList" >::: [
    "test_gen1" >:: test_gen1
  ; "test_gen2" >:: test_gen2
  ; "test_gen3" >:: test_gen3
  ; "test_from_list1" >:: test_from_list1
  ; "test_from_list2" >:: test_from_list2
  ; "test_from_list3" >:: test_from_list3
  ]

let () =
  run_test_tt_main suite