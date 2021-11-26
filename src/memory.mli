type memory = { l : int list; c : int; r : int list; ptr : int }
val print_memory : memory -> unit
val save_memory_to_file : memory -> string -> unit
