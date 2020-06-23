open OUnit2
open Kontlang

let () = Unix.chdir "../../.."

let test_rm1 _ =
  let s = "
  (let [R Stdlib.ReaderMonad]
    (R.run_reader
      (R.reify
        (let* [(x 10)
               (y (+ x (R.ask)))]
          y))
      5))
  " in
  assert_equal (Execute.eval_string s) "15"

let suite =
  "StdlibReaderMonadTestList" >::: [
    "test_rm1" >:: test_rm1
  ]

let () =
  run_test_tt_main suite