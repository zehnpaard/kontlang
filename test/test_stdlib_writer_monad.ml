open OUnit2
open Kontlang

let () = Unix.chdir "../../.."

let test_wm1 _ =
  let s = "
  (let [W (Stdlib.WriterMonad.make + 0)]
    (W.reify
      (do [(W.tell 5) 3])))
  " in
  assert_equal (Execute.eval_string s) "(3 . 5)"

let test_wm2 _ =
  let s = "
  (let [W (Stdlib.WriterMonad.make cons nil)]
    (W.reify
      (do [(W.tell \"hello\") (W.tell \"world\") 3])))
  " in
  assert_equal (Execute.eval_string s) "(3 \"hello\" \"world\")"

let suite =
  "StdlibWriterMonadTestList" >::: [
    "test_wm1" >:: test_wm1
  ; "test_wm2" >:: test_wm2
  ]

let () =
  run_test_tt_main suite