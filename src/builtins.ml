let num_num_op s op =
  let g = function
  | Val.Int n -> n
  | _ -> failwith @@ "Non-numeric value passed to numeric op " ^ s
  in
  let f xs = match List.map g xs with
  | [] -> failwith @@ Printf.sprintf "Numeric op %s applied to empty list" s
  | y::ys -> Val.Int (List.fold_left op y ys)
  in
  Val.Op (s, f)

let builtins =
[ "+", num_num_op "+" (+)
; "-", num_num_op "-" (-)
; "*", num_num_op "*" ( * )
; "/", num_num_op "/" (/)
]

let load env = Env.extend_list builtins env