type memory = { l : int list; c : int; r : int list; ptr : int }


let output_line idx idx_digits ncells lst ptr =
  let fixed_length length chr str =
    let amount = length - (String.length str) in
    if amount < 0 then (* well, can't fit it in... *)
      String.make length '?'
    else
      Printf.sprintf "%s%s" (String.make amount chr) str in

  (* print index *)
  Printf.printf "%s:  " (fixed_length idx_digits '0' (string_of_int idx));

  (* print cells *)
  let rec print_next i lst =
    if i = ncells then
      (lst, [])
    else
      let (l, ls) = match lst with
      | l'::ls' -> (l', ls')
      | [] -> (0, []) in
      let (c1, c2) = if ptr = i + idx then ('[', ']') else (' ',' ') in (* mark ptr location *)
      Printf.printf "%c%s%c" c1 (fixed_length 3 '0' (string_of_int l)) c2; (* FIXME: magic constant 3 *)
      let (rlst, cs) = print_next (i + 1) ls in (rlst, l::cs) in

  let (res_lst, cells) = print_next 0 lst in

  (* print char representation*)
  let convert_and_print i =
    let c = if i >= 32 && i <= 126 then
      Char.chr i
    else
      '.' in
    print_char c in
  
  print_string "  ";
  List.iter convert_and_print cells;
  print_endline "";
  res_lst


let print_memory mem =
  let ncells = 16 in
  let lidx = List.length mem.l in
  let lst = List.rev_append mem.l (mem.c::mem.r) in
  let total_elms = List.length lst in
  let max_idx_shown = total_elms - (total_elms mod ncells) in
  let idx_digits = String.length (string_of_int max_idx_shown) in
  
  (* header *)
  print_endline "\n";
  print_endline "Memory dump\n";
  print_endline ("Pointer: " ^ (string_of_int mem.ptr));
  print_endline "";

  let rec print_lines idx lst ncells =
    let res_lst = output_line idx idx_digits ncells lst mem.ptr in
    match res_lst with
    | [] -> ()
    | lst1 -> print_lines (idx + ncells) lst1 ncells in 
  
  print_lines (mem.ptr - lidx) lst ncells