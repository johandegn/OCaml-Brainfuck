open Tokens

let valid_symbol c =
  match c with
  | ',' | '.' | '+' | '-' | '<' | '>' | '[' | ']' -> true
  | _ -> false

let get_token c = 
  match c with
  | ',' -> Input
  | '.' -> Output
  | '+' -> Increase
  | '-' -> Decrease
  | '<' -> LeftShift
  | '>' -> RightShift
  | '[' -> OpenBracket
  | ']' -> CloseBracket
  | _ -> failwith "char array is not clean!" (* shorthand for Failure exception*)

let create_tokens str =
  let max = String.length str - 1 in
  let rec inner n tail =
    if n < 0 then tail else
    let c = str.[n] in
    if not @@ valid_symbol c then inner (n - 1) tail else
    get_token c :: tail |> inner (n - 1) in
  inner max []

let tokenize str =
  create_tokens str
