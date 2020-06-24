open OUnit2
open Kontlang

let () = Unix.chdir "../../.."

let test_gm1 _ =
  let s = "
  (let* [(G Stdlib.Generator)
         (GM Stdlib.GeneratorMonad)
         (g (GM.reify
              (let [(x (GM.reflect (G.from_list (list 1 2))))
                    (y (GM.reflect (G.from_list (list 3 4))))]
                (cons x y))))]
    (car (g)))
  " in
  assert_equal (Execute.eval_string s) "(1 . 3)"

let test_gm2 _ =
  let s = "
  (let* [(G Stdlib.Generator)
         (GM Stdlib.GeneratorMonad)
         (g (GM.reify
           (let [(x (GM.reflect (G.take 5 (G.countn 1))))
                 (y (GM.reflect (G.take 5 (G.countn 1))))
                 (z (GM.reflect (G.take 5 (G.countn 1))))]
             (if (= (* z z)
                    (+ (* x x)
                       (* y y)))
               (list x y z)
               (GM.fail)))))]
    (car (g)))
  " in
  assert_equal (Execute.eval_string s) "(3 4 5)"

let test_gm3 _ =
  let s = "
  (let* [(G Stdlib.Generator)
         (GM Stdlib.GeneratorMonad)
         (g (GM.reify
           (let* [(x (GM.reflect (G.take 5 (G.countn 1))))
                  (y (GM.reflect (G.take 5 (G.countn 1))))
                  (z (GM.reflect (G.take 5 (G.countn 1))))
                  (_ (GM.assert (= (* z z)
                                   (+ (* x x) (* y y)))))]
             (list x y z))))]
    (car (g)))
  " in
  assert_equal (Execute.eval_string s) "(3 4 5)"

let suite =
  "StdlibGeneratorMonadTestList" >::: [
    "test_gm1" >:: test_gm1
  ; "test_gm2" >:: test_gm2
  ; "test_gm3" >:: test_gm3
  ]

let () =
  run_test_tt_main suite