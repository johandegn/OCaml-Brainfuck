open Options

let program_path = ref ""
let program = ref ""
let input_path = ref ""
let input = ref ""
let dump_memory = ref false
let memory_dump_file = ref ""
let request_input = ref false
let no_flush = ref false
let print_exec_time = ref false
let options = ref {end_of_input = 0; request_input = false; always_flush = false}

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

(** Handle anonymous arguments *)
let handle_anonymous arg =
  if !program = "" && !program_path = "" then
    program_path := arg
  else if !input = "" && !input_path = "" then
    input := arg
  else
    raise (Arg.Bad ("Too many anonymous arguments"))


  let set_eoi i =
    options := {!options with end_of_input = i}


let speclist = [
    ("-c", Arg.String set_program, ": Program passed in as string.");
    ("-i", Arg.String set_input_path, ": Loads input from file.");
    ("-I", Arg.Set request_input, ": Request input if initial input is consumed. If none given, end-of-input value is used.");
    ("-e", Arg.Int set_eoi, ": End-of-input value. Default is 0.");
    ("-d", Arg.Set dump_memory, ": Dump memory after termination.");
    ("-D", Arg.Set_string memory_dump_file, ": Write memory dump to file after termination.");
    ("-p", Arg.Set no_flush, ": Do NOT flush output on each print.");
    ("-t", Arg.Set print_exec_time, ": Print execution time.");
  ]


let load_resources () =
  if not (!program_path = "") then program := Utility.read_file !program_path;
  if not (!input_path = "") then input := Utility.read_file !input_path;
  options := {!options with request_input = !request_input; always_flush = not !no_flush}


let run code input options = Interpreter.eval (Parser.parse (Lexer.tokenize code)) (Utility.encode_input input) options;;


let print_time_diff start stop =
  let millisec_diff = ((stop -. start) *. 1000.) in
  let millisecs = (int_of_float millisec_diff) mod 1000 in
  let secs = (int_of_float millisec_diff / 1000) mod 60 in
  let mins = int_of_float millisec_diff / 60000 in
  Printf.printf "\n\nExecution time: %dm %ds %dms (total: %fms)\n" mins secs millisecs millisec_diff;
  flush stdout


let () =
  Arg.parse speclist handle_anonymous usage;
  load_resources ();
  let start_time = Unix.gettimeofday () in (* execution timing *)
  let mem = run !program !input !options in ();
  let stop_time = Unix.gettimeofday () in (* execution timing *)
  if !dump_memory then Memory.print_memory mem; (* print memory dump *)
  if !print_exec_time then print_time_diff start_time stop_time;
  if not (!memory_dump_file = "") then Memory.save_memory_to_file mem !memory_dump_file (* save memory dump to a file *)