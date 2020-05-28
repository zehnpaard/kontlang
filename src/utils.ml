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

let break_off n xs =
  let rec f n acc xs =
    if n = 0 then List.rev acc, xs
    else match xs with
    | [] -> failwith "Not enough elements in list to break off"
    | x::xs' -> f (n-1) (x::acc) xs'
  in f n [] xs