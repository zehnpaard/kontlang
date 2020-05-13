type t =
| Int of int
| Var of string

let to_string = function
| Int n -> string_of_int n
| Var s -> s