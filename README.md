# kontlang
Language with Clojure-like syntax and semantics based on EoPL + Shift/Reset, implemented in OCaml

## Instructions for use
Recommendation for installing dependencies is [`OPAM`](https://opam.ocaml.org).

Once OPAM is installed and configured (`opam init` & `eval $(opam config env)` - see OPAM website for further details), run:

```
opam install dune menhir ounit2
```

This will install all dependencies for kontlang.

Then in the `kontlang` repo:

```
# Running a Read-Eval-Print interpreter
dune exec ./main.exe
```

or

```
# Running a Stepwise Execution interpreter
dune exec ./stepwise.exe
```


# Example programs

Based on examles and exercises from ["Introduction to Programming with Shift and Reset"](http://pllab.is.ocha.ac.jp/~asai/cw2011tutorial/main-e.pdf)

Non-deterministic search of numbers matching Pythagorus' Theorem

```racket
(letrec
  [doall [f xs]
    (if (nil? xs)
      nil
      (do [(f (car xs))
           (doall f (cdr xs))]))]
  (letfn [choice [xs]
           (shift [k] (doall k xs))]
    (reset
      (let* [(lst (list 1 2 3 4 5))
             (x (choice lst))
             (y (choice lst))
             (z (choice lst))]
        (if (= (* z z)
               (+ (* x x)
                  (* y y)))
          (println
            (concat (to_string x) (to_string y) (to_string z)))
          nil)))))
```

Above returns

```
345
435
nil
```

State monad:

```racket
(letfn
  [(get []
     (shift [k]
       (fn [state] ((k state) state))))
   (tick []
     (shift [k]
       (fn [state] ((k nil) (+ state 1)))))
   (run_state [thunk]
     ((reset 
        (let [result (thunk)]
          (fn [state] result))) 0))]
  (run_state
    (fn []
      (do [(tick)
           (tick)
           (let [a (get)]
              (do [(tick)
                   (tick)
                   (cons (get) (- (get) a))]))]))))
```

returns the cons'ed result of the final state and the difference between it and the intermediate state captured by `let [a (get)]`:

```
(4 . 2)
```
