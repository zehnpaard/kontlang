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

let num_bool_op s op =
  let g = function
  | Val.Int n -> n
  | _ -> failwith @@ "Non-numeric value passed to numeric op " ^ s
  in
  let rec h op = function
  | [] | [_] -> Val.Bool true
  | x::y::ys -> if op x y then h op @@ y::ys else Val.Bool false
  in
  let f xs = match List.map g xs with
  | [] -> failwith @@ Printf.sprintf "NumBool op %s applied to empty list" s
  | [_] -> failwith @@ Printf.sprintf "NumBool op %s applied to one arg" s
  | ys -> h op ys
  in
  Val.Op (s, f)

let builtins =
[ "+", num_num_op "+" (+)
; "-", num_num_op "-" (-)
; "*", num_num_op "*" ( * )
; "/", num_num_op "/" (/)
; "true", Val.Bool true
; "false", Val.Bool false
; "=", num_bool_op "=" (=)
; "!=", num_bool_op "!=" (!=)
; ">", num_bool_op ">" (>)
; ">=", num_bool_op ">=" (>=)
; "<", num_bool_op "<" (<)
; "<=", num_bool_op "<=" (<=)
]

let load env = Env.extend_list builtins env