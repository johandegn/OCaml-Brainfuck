exception Parse of string

open Tokens

let paranteses_match token_lst = 
  let rec inner lst balance =
    if balance < 0 then false else
    match lst with
    | OpenBracket::l -> inner l (balance + 1)
    | CloseBracket::l -> inner l (balance - 1)
    | _::l -> inner l balance
    | [] -> if balance = 0 then true else false in
  inner token_lst 0

let rec gen_ast token_lst =
  let rec count_vals lst = 
    match lst with
    | Increase::l -> let (c, rl) = (count_vals l) in (c + 1, rl)
    | Decrease::l -> let (c, rl) = (count_vals l) in (c - 1, rl)
    | _ -> (0, lst) in

  let rec count_shifts lst = 
    match lst with
    | RightShift::l -> let (c, rl) = (count_shifts l) in (c + 1, rl)
    | LeftShift::l -> let (c, rl) = (count_shifts l) in (c - 1, rl)
    | _ -> (0, lst) in

  let loop_tail lst =
    let rec inner lst c = 
      match lst with
      | OpenBracket::l -> inner l (c + 1)
      | CloseBracket::l -> if c = 0 then l else inner l (c - 1)
      | _::l -> inner l c
      | _ -> failwith "This cannot happen, unless its forgotten to check that paranteses match.." in
    inner lst 0 in

  match token_lst with
  | Increase::_
  | Decrease::_ ->
    let (c, lr) = count_vals token_lst in
    Nodes.ChangeVal c :: gen_ast lr
  | RightShift::_
  | LeftShift::_ ->
    let (c, lr) = count_shifts token_lst in
    Nodes.ChangePtr c :: gen_ast lr
  | OpenBracket::l -> Nodes.Loop (gen_ast l) :: gen_ast (loop_tail l)
  | CloseBracket::_ -> []
  | Input::l -> Nodes.InputValue :: gen_ast l
  | Output::l -> Nodes.PrintValue :: gen_ast l
  | _ -> []
  

let parse token_lst = 
  if not @@ paranteses_match token_lst then raise (Parse "Mismatching parenteses");
  gen_ast token_lst
