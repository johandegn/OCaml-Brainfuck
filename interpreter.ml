let lists_from_ptr lst ptr = 
  if ptr < 0 then failwith "Pointer is negative" else
  
  let corrected_list lst =
    match lst with
    | [] -> 0::[]
    | _::_ -> lst in
  
  let rec inner lst ptr = 
    let clst = (corrected_list lst) in
    if ptr == 0 then ([], clst) else
    match clst with
    | e::l1 -> let (l2, l3) = inner l1 (ptr - 1) in (e::l2, l3)
    | _ -> failwith "List was empty. This should not be able to happen" in (* shut up OCaml warnings *)

  inner lst ptr

let rec change_val lst ptr n =
  let (lst1, lst2) = lists_from_ptr lst ptr in
  match lst2 with
  | v::lst3 -> lst1 @ (v + n)::lst3
  | _ -> failwith "List was empty. This should not be able to happen" (* shut up OCaml warnings *)

let rec print_val lst ptr = 
  let (lst1, lst2) = lists_from_ptr lst ptr in
  match lst2 with
  | v::_-> if v < 0 || v > 127 then failwith "Value out of bounds" else print_char (Char.chr v)
  | _ -> failwith "List was empty. This should not be able to happen" (* shut up OCaml warnings *)

let check_value lst ptr n =
  let (lst1, lst2) = lists_from_ptr lst ptr in
  match lst2 with
  | v::_ -> if n == v then true else false
  | _ -> failwith "List was empty. This should not be able to happen" (* shut up OCaml warnings *)

let rec interpret lst ptr ast =
  match ast with
  | Nodes.Tuple (ast1, ast2) -> 
    let (new_lst, new_ptr) = interpret lst ptr ast1 in
    interpret new_lst new_ptr ast2
  | Nodes.Loop ast1 ->
    if (check_value lst ptr 0) then (lst, ptr) else
    let (new_lst, new_ptr) = interpret lst ptr ast1 in
    interpret new_lst new_ptr ast
  | Nodes.ChangeVal n -> ((change_val lst ptr n), ptr)
  | Nodes.ChangePtr n -> (lst, ptr + n)
  | Nodes.InputValue -> failwith "not implemented yet"
  | Nodes.PrintValue -> print_val lst ptr; (lst, ptr)
  | Nop -> (lst, ptr)

let eval ast = interpret [] 0 ast;;