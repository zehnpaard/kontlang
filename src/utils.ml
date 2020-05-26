let split3 xyzs =
  let rec f xs ys zs = function
  | [] -> xs, ys, zs
  | (x,y,z)::xyzs -> f (x::xs) (y::ys) (z::zs) xyzs
  in
  f [] [] [] xyzs

let dedupe xs =
  let hsh = Hashtbl.create 1024 in
  let rec f acc = function
  | [] -> acc
  | x::xs' ->
    if Hashtbl.mem hsh x then f acc xs'
    else (Hashtbl.add hsh x 0; f (x::acc) xs')
  in
  f [] xs

let rec drop_last = function
| [] | [_] -> []
| x::xs -> x :: (drop_last xs)