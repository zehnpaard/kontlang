# kontlang
Language with Clojure-like syntax and semantics based on EoPL + Shift/Reset, implemented in OCaml

# Language features
Kontlang is a dynamically typed, purely functional (excluding the use of continuations) language interpreted with a CEK-machine-like evaluator, supporting the following features:

* `let`, `let*`, `letfn`, `letrec` (each supporting multiple variable bindings in one expression, and mutual recursion in the case of `letrec`)
* lexical scope functions with closures
* tail call optimized
* simple dynamic scope macros with expression substitution
* first class modules that can be imported from file
* delimited continuations via the `shift` & `reset` operators
* standard library modules including implementation of monads via `reify` and `reflect` operators

## Instructions for use
Recommendation for installing dependencies is [`OPAM`](https://opam.ocaml.org), the defacto standard OCaml package manager.

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

Based on examples and exercises from ["Introduction to Programming with Shift and Reset"](http://pllab.is.ocha.ac.jp/~asai/cw2011tutorial/main-e.pdf) together with `reify` and `reflect` operators as defined in Andrzej Filinski's Representing Monads paper.

Non-deterministic search of numbers matching Pythagorus' Theorem

```racket
(let [L Stdlib.ListMonad]
  (L.reify
    (let [(x (L.reflect (list 1 2 3 4 5)))
          (y (L.reflect (list 1 2 3 4 5)))
          (z (L.reflect (list 1 2 3 4 5)))]
      (if (= (* z z)
             (+ (* x x)
                (* y y)))
        (list x y z)
        (L.fail)))))
```

Above returns `((3 4 5) (4 3 5))`

State monad:

```racket
(let [S Stdlib.StateMonad]
  (S.run_state
    (S.reify
      (let* [(tick
               (fn []
                 (S.reflect
                   (fn [state] (cons nil (+ state 1))))))
             (_ (tick))
             (_ (tick))
             (a (S.get))
             (_ (tick))]
         (- (S.get) a)))
    0))
```

returns a tuple of the "result" and the final state `(1 . 3)`

For more usage examples, see the `/test` directory.
