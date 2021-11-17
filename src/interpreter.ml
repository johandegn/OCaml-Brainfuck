open Memory
open Options
open Nodes

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


let print_value v always_flush =
  if v < 0 || v >= 256 then raise (Value "Value out of bounds")
  else print_char (Char.chr v); if always_flush then flush stdout


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


let interpret mem inp ins_lst opts =
  let rec interpret mem inp ins_lst =
    match ins_lst with
    | [] -> (mem, inp)
    | ins::rest -> (
      let (mem, inp) = match ins with
      | Loop body_lst ->
        if mem.c = 0 then (mem, inp) 
        else let (mem, inp) = interpret mem inp body_lst in
        interpret mem inp (ins::[])
      | ChangeVal n -> check_pointer mem; ({mem with c = mem.c + n}, inp)
      | ChangePtr n -> (shift_memory mem n, inp)
      | InputValue -> check_pointer mem; handle_input mem inp opts
      | PrintValue -> check_pointer mem; print_value mem.c opts.always_flush; (mem, inp) in
      
      interpret mem inp rest) in
  
  interpret mem inp ins_lst


let eval ins_lst inp opts =
  let (mem, _) = interpret {l = []; c = 0; r = []; ptr = 0} inp ins_lst opts in
  mem
