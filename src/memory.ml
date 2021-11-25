type memory = { l : int list; c : int; r : int list; ptr : int }


let output_line idx idx_digits ncells lst ptr out_ch =
  let fixed_length length chr num =
    let str1 = string_of_int (abs num) in
    let real_length = length - (if num < 0 then 1 else 0) in
    let amount = real_length - (String.length str1) in
    let str2 = if amount < 0 then (* well, can't fit it in... *)
      String.make real_length '?'
    else
      Printf.sprintf "%s%s" (String.make amount chr) str1 in
    if num < 0 then
      "-" ^ str2
    else
      str2 in

  (* print index *)
  Printf.fprintf out_ch "%s:  " (fixed_length idx_digits '0' idx);

  (* print cells *)
  let rec print_next i lst =
    if i = ncells then
      (lst, [])
    else
      let (l, ls) = match lst with
      | l'::ls' -> (l', ls')
      | [] -> (0, []) in
      let (c1, c2) = if ptr = i + idx then ('[', ']') else (' ',' ') in (* mark ptr location *)
      Printf.fprintf out_ch "%c%s%c" c1 (fixed_length 3 '0' l) c2; (* FIXME: magic constant 3 *)
      let (rlst, cs) = print_next (i + 1) ls in (rlst, l::cs) in

  let (res_lst, cells) = print_next 0 lst in

  (* print char representation*)
  let convert_and_print i =
    let c = if i >= 32 && i <= 126 then
      Char.chr i
    else
      '.' in
    Printf.fprintf out_ch "%c" c in
  
  Printf.fprintf out_ch "  ";
  List.iter convert_and_print cells;
  Printf.fprintf out_ch "\n";
  res_lst


let output_memory mem out_ch =
  let ncells = 15 in
  let lidx = List.length mem.l in
  let lst = List.rev_append mem.l (mem.c::mem.r) in
  let total_elms = List.length lst in
  let max_idx_shown = total_elms - (total_elms mod ncells) in
  let idx_digits = String.length (string_of_int max_idx_shown) in
  
  (* header *)
  Printf.fprintf out_ch "Memory dump\n\n";
  Printf.fprintf out_ch "%s%d" "Pointer: " mem.ptr;
  Printf.fprintf out_ch "\n";

  (* content *)
  let rec print_lines idx lst ncells =
    let res_lst = output_line idx idx_digits ncells lst mem.ptr out_ch in
    match res_lst with
    | [] -> ()
    | lst1 -> print_lines (idx + ncells) lst1 ncells in 
  
  print_lines (mem.ptr - lidx) lst ncells

(** Prints memory to stdout *)
let print_memory mem =
  Printf.printf "\n\n"; (* distance to header *)
  output_memory mem stdout

(** Prints memory to a file *)
let save_memory_to_file mem filename =
  let oc = open_out filename in (* create or truncate file, return channel *)
  output_memory mem oc;
  close_out oc;