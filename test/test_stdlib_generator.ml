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
    ((cdr (g))))
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
    ((cdr (g))))
  " in
  assert_equal (Execute.eval_string s) "nil"

let test_concat1 _ =
  let s = "
  (let* [(G Stdlib.Generator)
         (g (G.from_list (list 1 2)))
         (h (G.from_list (list 3 4)))]
    (G.to_list (G.concat g h)))
  " in
  assert_equal (Execute.eval_string s) "(1 2 3 4)"

let test_merge1 _ =
  let s = "
  (let* [(G Stdlib.Generator)
         (g (G.from_list (list 1 2)))
         (h (G.from_list (list 3 4)))
         (i (G.generator (do [(G.yield g) h])))]
    (G.to_list (G.merge i)))
  " in
  assert_equal (Execute.eval_string s) "(1 2 3 4)"

let test_map1 _ =
  let s = "
  (let* [(G Stdlib.Generator)
         (g (G.from_list (list 1 2 3 4 5)))]
    (G.to_list (G.map (fn [x] (* x x)) g)))
  " in
  assert_equal (Execute.eval_string s) "(1 4 9 16 25)"

let test_take1 _ =
  let s = "
  (let* [(G Stdlib.Generator)
         (g (G.from_list (list 1 2 3 4 5)))]
    (G.to_list (G.take 3 g)))
  " in
  assert_equal (Execute.eval_string s) "(1 2 3)"

let test_drop1 _ =
  let s = "
  (let* [(G Stdlib.Generator)
         (g (G.from_list (list 1 2 3 4 5)))]
    (G.to_list (G.drop 3 g)))
  " in
  assert_equal (Execute.eval_string s) "(4 5)"

let suite =
  "GeneratorTestList" >::: [
    "test_gen1" >:: test_gen1
  ; "test_gen2" >:: test_gen2
  ; "test_gen3" >:: test_gen3
  ; "test_from_list1" >:: test_from_list1
  ; "test_from_list2" >:: test_from_list2
  ; "test_from_list3" >:: test_from_list3
  ; "test_concat1" >:: test_concat1
  ; "test_merge1" >:: test_merge1
  ; "test_map1" >:: test_map1
  ; "test_take1" >:: test_take1
  ; "test_drop1" >:: test_drop1
  ]

let () =
  run_test_tt_main suite