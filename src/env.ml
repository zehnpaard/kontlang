type t = (string * Val.t) list

let empty = []

let extend var val_ env = (var, val_)::env
let extend_list vvs env = vvs @ env