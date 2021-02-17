open Options

let program_path = ref ""
let program = ref ""
let input_path = ref ""
let input = ref ""
let dump_memory = ref false
let options = ref {end_of_input = 0}

let usage = "usage: " ^ Sys.argv.(0) ^ " [-c cmd | file] [-i file | input] [options]"


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


  let set_eoi c =
    let len = String.length c in
    if len <> 1 then 
      raise (Arg.Bad "End-of-input should be a single char")
    else options := {!options with end_of_input = (Char.code c.[0])}



let speclist = [
    ("-c", Arg.String set_program, ": Program passed in as string.");
    ("-i", Arg.String set_input_path, ": Loads input from file.");
    ("-e", Arg.String set_eoi, ": End-of-input char. Default is 0.");
    ("-d", Arg.Set dump_memory, ": Dump memory after termination.");
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
    let s = really_input_string ch (in_channel_length ch - 1) in (* TODO: does -1 remove newline char, charied return, return or somthing?*)
    close_in ch;
    s
  with e ->
    close_in_noerr ch; (* emergency closing *)
    raise e;;


let load_resources () =
  if not (!program_path = "") then program := read_file !program_path;
  if not (!input_path = "") then input := read_file !input_path


let run code input options = Interpreter.eval (Parser.parse (Lexer.tokenize code)) (encode_input input) options;;


let () =
  Arg.parse speclist handle_anonymous usage;
  load_resources ();
  let mem = run !program !input !options in ();
  if !dump_memory then Memory.print_memory mem