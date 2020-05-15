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

let not_equal_op =
  let g = function
  | Val.Int n -> n
  | _ -> failwith "Non-numeric value passed to numeric op !="
  in
  let rec h = function
  | [] | [_] -> Val.Bool true
  | x::y::ys -> if x != y then h @@ y::ys else Val.Bool false
  in
  let f xs = match List.sort compare @@ List.map g xs with
  | [] -> failwith "NumBool op != applied to empty list"
  | [_] -> failwith "NumBool op != applied to one arg"
  | ys -> h ys
  in
  Val.Op ("!=", f)

let bool_bool_op s op =
  let g = function
  | Val.Bool b -> b
  | _ -> failwith @@ "Non-boolean value passed to bool op " ^ s
  in
  let f xs = Val.Bool (List.fold_left (fun x y -> op x @@ g y) true xs) in
  Val.Op (s, f)

let not_op = function
  | [Val.Bool b] -> Val.Bool (not b)
  | [_] -> failwith "Non-boolean value passed to bool uniop not_op"
  | _ -> failwith "Bool uniop not_op applied to more than one arg"

let builtins =
[ "+", num_num_op "+" (+)
; "-", num_num_op "-" (-)
; "*", num_num_op "*" ( * )
; "/", num_num_op "/" (/)
; "true", Val.Bool true
; "false", Val.Bool false
; "=", num_bool_op "=" (=)
; "!=", not_equal_op
; ">", num_bool_op ">" (>)
; ">=", num_bool_op ">=" (>=)
; "<", num_bool_op "<" (<)
; "<=", num_bool_op "<=" (<=)
; "and", bool_bool_op "and" (&&)
; "or", bool_bool_op "or" (||)
; "not", Val.Op("not_op", not_op)
]

let load env = Env.extend_list builtins env