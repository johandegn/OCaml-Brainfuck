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

let rec generate_ast token_lst =

  let count inc dec lst =
    let rec count c lst =
      match lst with
      | t::l when t = inc -> count (c + 1) l
      | t::l when t = dec -> count (c - 1) l
      | _ -> (c, lst) in
    count 0 lst in

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
    let (c, lst) = count Increase Decrease token_lst in
    Nodes.ChangeVal c :: generate_ast lst
  | RightShift::_
  | LeftShift::_ ->
    let (c, lst) = count RightShift LeftShift token_lst in
    Nodes.ChangePtr c :: generate_ast lst
  | OpenBracket::l -> Nodes.Loop (generate_ast l) :: generate_ast (loop_tail l)
  | CloseBracket::_ -> []
  | Input::l -> Nodes.InputValue :: generate_ast l
  | Output::l -> Nodes.PrintValue :: generate_ast l
  | _ -> []
  

let parse token_lst = 
  if not @@ paranteses_match token_lst then raise (Parse "Mismatching parenteses");
  generate_ast token_lst
