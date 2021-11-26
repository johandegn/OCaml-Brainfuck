let encode_input str =
  let max = String.length str in
  let rec encode i =
    if i = max then [] 
    else (Char.code str.[i])::(encode (i + 1)) in
  encode 0


let read_file filename =
  let ch = open_in filename in
  try
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s
  with e ->
    close_in_noerr ch; (* emergency closing *)
    raise e
