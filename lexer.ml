let rec create_tokens carr = 
  let get_token c = 
    match c with
    | ',' -> Tokens.Input
    | '.' -> Tokens.Output
    | '+' -> Tokens.Increase
    | '-' -> Tokens.Decrease
    | '<' -> Tokens.LeftShift
    | '>' -> Tokens.RightShift
    | '[' -> Tokens.OpenBracket
    | ']' -> Tokens.CloseBracket
    | _ -> failwith "char array is not clean!" in (* shorthand for Failure exception*)
  match carr with
  | c::l -> (get_token c)::(create_tokens l)
  | [] -> []

let valid_symbol c =
  match c with
  | ',' -> true
  | '.' -> true
  | '+' -> true
  | '-' -> true
  | '<' -> true
  | '>' -> true
  | '[' -> true
  | ']' -> true
  | _ -> false
  
let cleanup str =
  let max = String.length str - 1 in
  let rec clean i =
    let c = str.[i] in
    let tail = (if i == max then [] else (clean (i + 1))) in
    if valid_symbol c then c::tail else tail in
  clean 0

let tokenize str =
  create_tokens (cleanup str);;