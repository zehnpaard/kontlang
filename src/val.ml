type t =
| Int of int
| Bool of bool
| Op of string * (t list -> t)

let to_string = function
| Int n -> string_of_int n
| Bool b -> string_of_bool b
| Op (s, _) -> Printf.sprintf "Op(%s)" s