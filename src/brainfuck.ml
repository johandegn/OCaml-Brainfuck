let program_path = ref ""
let program = ref ""
let input_path = ref ""
let input = ref ""

let usage = "usage: " ^ Sys.argv.(0) ^ " [-c cmd | file] [-i file | input]"


let set_input_path str =
  if !input = "" then
    input_path := str
  else
    raise (Arg.Bad ("No more than a single input argument is allowed"))


let set_program str =
  if !program_path = "" then
    program := str
  else
    raise (Arg.Bad ("Only a single program argument is allowed"))


let handle_anonymous arg =
  if !program = "" && !program_path = "" then
    program_path := arg
  else if !input = "" && !input_path = "" then
    input := arg
  else
    raise (Arg.Bad ("Too many anonymous arguments"))


let speclist = [
    ("-c", Arg.String set_program, ": program passed in as string");
    ("-i", Arg.String set_input_path, ": loads input from file");
  ]


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
    raise e;;


let load_resources () =
  if not (!program_path = "") then program := read_file !program_path;
  if not (!input_path = "") then input := read_file !input_path


let run code input = Interpreter.eval (Parser.parse (Lexer.tokenize code)) (encode_input input);;


let () =
  Arg.parse speclist handle_anonymous usage;
  load_resources ();
  let _ = run !program !input in ()