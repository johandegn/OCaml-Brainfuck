






let paranteses_match token_lst = 
  let rec inner lst balance =
    if balance < 0 then false else
    match lst with
    | Tokens.OpenBracket::l -> inner l (balance + 1)
    | Tokens.CloseBracket::l -> inner l (balance - 1)
    | _::l -> inner l balance
    | [] -> if balance == 0 then true else false in
  inner token_lst 0;;

let parse token_lst = 
  if not (paranteses_match token_lst) then failwith "Mismatching parenteses";
  
  Nodes.ChangeVal 7;;


parse (Lexer.tokenize "abc 829374 +.,-[[]asdf[asdf]f asfd] <, > dsf");; (* why no work?? *)
