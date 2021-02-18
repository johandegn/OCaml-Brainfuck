let encode_input str =
  let max = String.length str in
  let rec encode i =
    if i = max then [] 
    else (Char.code str.[i])::(encode (i + 1)) in
  encode 0