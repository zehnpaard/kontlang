type t =
| Int of int
| Op of string * (t list -> t)

let to_string = function
| Int n -> string_of_int n
| Op (s, _) -> Printf.sprintf "Op(%s)" s