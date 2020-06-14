open OUnit2
open Kontlang

let () = Unix.chdir "../../.."

let test_rev1 _ =
  let s = "(Stdlib.List.rev (list 1 2 3 4 5))" in
  assert_equal (Execute.eval_string s) "(5 4 3 2 1)"

let test_rev2 _ =
  let s = "(Stdlib.List.rev nil)" in
  assert_equal (Execute.eval_string s) "nil"

let test_map1 _ =
  let s = "(Stdlib.List.map (fn [x] (* x x)) (list 1 2 3 4 5))" in
  assert_equal (Execute.eval_string s) "(1 4 9 16 25)"

let test_map2 _ =
  let s = "(Stdlib.List.map (fn [x] (* x x)) nil)" in
  assert_equal (Execute.eval_string s) "nil"

let test_fold_left1 _ =
  let s = "(Stdlib.List.fold_left (fn [x y] (+ (* 10 x) y)) 0 (list 1 2 3))" in
  assert_equal (Execute.eval_string s) "123"
  
let test_fold_left2 _ =
  let s = "(Stdlib.List.fold_left (fn [x y] (+ (* 10 x) y)) 0 nil)" in
  assert_equal (Execute.eval_string s) "0"

let test_range1 _ =
  let s = "(Stdlib.List.range 5)" in
  assert_equal (Execute.eval_string s) "(0 1 2 3 4)"
  
let test_range2 _ =
  let s = "(Stdlib.List.range 0)" in
  assert_equal (Execute.eval_string s) "nil"
  
let test_range3 _ =
  let s = "(Stdlib.List.range -1)" in
  assert_equal (Execute.eval_string s) "nil"

let test_find1 _ =
  let s = "(Stdlib.List.find (fn [x] (= x 2)) (list 1 2 3 4 5))" in
  assert_equal (Execute.eval_string s) "2"
  
let test_find2 _ =
  let s = "(Stdlib.List.find (fn [x] (= x 2)) (list 1 3 4 5))" in
  assert_equal (Execute.eval_string s) "nil"
  
let suite =
  "StdlibListTestList" >::: [
    "test_rev1" >:: test_rev1
  ; "test_rev2" >:: test_rev2
  ; "test_map1" >:: test_map1
  ; "test_map2" >:: test_map2
  ; "test_fold_left1" >:: test_fold_left1
  ; "test_fold_left2" >:: test_fold_left2
  ; "test_range1" >:: test_range1
  ; "test_range2" >:: test_range2
  ; "test_range3" >:: test_range3
  ; "test_find1" >:: test_find1
  ; "test_find2" >:: test_find2
  ]

let () =
  run_test_tt_main suite