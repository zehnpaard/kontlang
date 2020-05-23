let split3 xyzs =
  let rec f xs ys zs = function
  | [] -> xs, ys, zs
  | (x,y,z)::xyzs -> f (x::xs) (y::ys) (z::zs) xyzs
  in
  f [] [] [] xyzs