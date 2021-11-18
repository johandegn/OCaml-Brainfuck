open Tokens

exception Parse of string

let paranteses_match token_lst = 
  let rec inner lst balance =
    if balance < 0 then false else
    match lst with
    | OpenBracket::l -> inner l (balance + 1)
    | CloseBracket::l -> inner l (balance - 1)
    | _::l -> inner l balance
    | [] -> if balance = 0 then true else false in
  inner token_lst 0

let generate_ast (options: Options.parse_options) token_lst =
  let count inc dec lst =
    let rec count c lst =
      match lst with
      | t::l when t = inc -> if options.optimize then count (c + 1) l else (1, l)
      | t::l when t = dec -> if options.optimize then count (c - 1) l else (-1, l)
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
  
  let rec generate_ast token_lst =
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
    | _ -> [] in
  generate_ast token_lst

let parse options token_lst = 
  if not @@ paranteses_match token_lst then raise (Parse "Mismatching parenteses");
  generate_ast options token_lst
