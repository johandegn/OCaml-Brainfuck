open Memory
open Options

exception Pointer of string
exception Value of string

let shift_memory mem am =
  let fix_lst lst = (* TODO: Also remove 0 if they are leading or tailing to save memory? *)
    match lst with
    | [] -> 0::[]
    | _ -> lst in
  
  let positive = am > 0 in
  let rec inner mem i =
    if i = 0 then mem else
    let (new_l, new_c, new_r) = if positive then
    match (fix_lst mem.r) with
    | c::rs -> (mem.c::mem.l, c, rs)
    | [] -> failwith "List was empty. This should not be able to happen" else
    match (fix_lst mem.l) with
    | c::ls -> (ls, c, mem.c::mem.r)
    | [] -> failwith "List was empty. This should not be able to happen" in
    inner {mem with l = new_l; r = new_r; c = new_c} (i - 1) in
  let new_mem = inner mem (abs am) in {new_mem with ptr = mem.ptr + am}


(** Raises Failure exception if pointer is out of bounds *)
let check_pointer mem =
  if mem.ptr < 0 then raise (Pointer "Pointer out of bounds") else ()


let print_value v =
  if v < 0 || v >= 256 then raise (Value "Value out of bounds")
  else print_char (Char.chr v); flush stdout


let get_inp () =
  Utility.encode_input (read_line ())


let handle_input mem inp opts =
  let (v, inp1) =
  match inp with
  | v::inp1 -> (v, inp1)
  | [] -> if opts.request_input then 
    match (get_inp ()) with
    | v::inp1 -> (v, inp1)
    | [] -> (opts.end_of_input, inp)
  else 
    (opts.end_of_input, inp) in
  ({mem with c = v}, inp1)


let interpret mem ast inp opts =
  let rec interpret mem ast inp =
    match ast with
    | Nodes.Tuple (ast1, ast2) ->
      let (mem1, inp1) = interpret mem ast1 inp in
      interpret mem1 ast2 inp1
    | Nodes.Loop ast1 ->
      if mem.c = 0 then (mem, inp) else 
      let (mem1, inp1) = interpret mem ast1 inp in
      interpret mem1 ast inp1
    | Nodes.ChangeVal n -> check_pointer mem; ({mem with c = mem.c + n}, inp)
    | Nodes.ChangePtr n -> (shift_memory mem n, inp)
    | Nodes.InputValue -> check_pointer mem; handle_input mem inp opts
    | Nodes.PrintValue -> check_pointer mem; print_value mem.c; (mem, inp)
    | Nodes.Nop -> (mem, inp) in
    interpret mem ast inp


let eval ast inp opts =
  let (mem, res_inp) = interpret {l = []; c = 0; r = []; ptr = 0} ast inp opts in
  mem
