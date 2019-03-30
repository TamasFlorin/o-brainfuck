type instruction = IncDataPointer | DecDataPointer | IncByte | DecByte | OutByte | AccByte | JmpForward | JmpBackward;;
type pointer = int;;
type instruction_index = int;;
type program_state = bytes * pointer * instruction_index;;
type program = instruction list;;

let add_byte (table: bytes) (ptr: pointer) (value: int): unit = 
  let current_byte = Bytes.get table ptr in 
    Bytes.set table ptr (Char.chr (value + (Char.code current_byte)));;

let set_byte (table: bytes) (ptr: pointer) (value: char): unit = 
  Bytes.set table ptr value;;

let increment_byte (table: bytes) (ptr: pointer): unit = 
  add_byte table ptr 1;;

let decrement_byte (table: bytes) (ptr: pointer): unit = 
  add_byte table ptr (-1);;

let get_byte_at_data_pointer (p: program_state): char = 
  let (table, ptr, i) = p in Bytes.get table ptr;;

let increment_data_pointer (p: program_state): program_state = 
  let (table, ptr, i) = p in (table, ptr + 1, i);;

let decrement_data_pointer (p: program_state): program_state = 
  let (table, ptr, i) = p in (table, ptr - 1, i);;

let increment_byte_at_data_pointer (p: program_state): program_state = 
  let (table, ptr, i) = p in 
    increment_byte table ptr;
    (table, ptr, i);;

let decrement_byte_at_data_pointer (p: program_state): program_state = 
  let (table, ptr, i) = p in 
    decrement_byte table ptr;
    (table, ptr, i);;

let output_byte_at_data_pointer (p: program_state): program_state = 
  let byte = get_byte_at_data_pointer p in 
    let () = print_char byte in p;;

let accept_byte_at_data_pointer (p: program_state): program_state = 
  let (table, ptr, i) = p in
    let character = Char.chr (read_int()) in 
      let () = set_byte table ptr character in p;;
  
let advance_statement_index (p: program_state): program_state = 
  let (table, ptr, index) = p in 
    (table, ptr, index + 1);;

let dec_statement_index (p: program_state): program_state = 
  let (table, ptr, index) = p in 
    (table, ptr, index - 1);;

let get_current_instruction (prog: program) (state: program_state): instruction = 
  let (_, _, index) = state in
    List.nth prog index;;

let jump_forward_to_matching_instruction (prog: program) (state: program_state): program_state = 
  let rec jmp (prog: program) (state: program_state) (count: int): program_state = 
    match count > 0 with
    | false -> state
    | true -> 
      match get_current_instruction prog state with
      | JmpForward -> jmp prog (advance_statement_index state) (count + 1)
      | JmpBackward -> jmp prog (advance_statement_index state) (count - 1)
      | _ ->  jmp prog (advance_statement_index state) count
  in
    jmp prog (advance_statement_index state) 1;;

let jump_backward_to_matching_instruction (prog: program) (state: program_state): program_state = 
  let rec jmp (prog: program) (state: program_state) (count: int): program_state = 
    match count > 0 with
    | false -> state
    | true -> 
      match get_current_instruction prog state with
      | JmpBackward -> jmp prog (dec_statement_index state) (count + 1)
      | JmpForward -> jmp prog (dec_statement_index state) (count - 1)
      | _ ->  jmp prog (dec_statement_index state) count
  in
  jmp prog (dec_statement_index state) 1;;

let jump_forward (prog: program) (state: program_state): program_state = 
  let byte = get_byte_at_data_pointer state in
    match Char.code(byte) with 
    | 0 -> jump_forward_to_matching_instruction prog state
    | _ -> state;;

let jump_backward (prog: program) (state: program_state): program_state = 
  let byte = get_byte_at_data_pointer state in
    match Char.code(byte) with 
    | 0 -> state
    | _ -> jump_backward_to_matching_instruction prog state;;

let execute_current_instruction (prog: program) (p: program_state): program_state = 
  match get_current_instruction prog p with 
  | IncDataPointer -> increment_data_pointer p
  | DecDataPointer -> decrement_data_pointer p
  | IncByte -> increment_byte_at_data_pointer p
  | DecByte -> decrement_byte_at_data_pointer p 
  | OutByte -> output_byte_at_data_pointer p
  | AccByte -> accept_byte_at_data_pointer p
  | JmpForward -> jump_forward prog p
  | JmpBackward -> jump_backward prog p;;

let get_program_table : bytes = 
  let table_size = 30000 in
    Bytes.make table_size (Char.chr 0);;

let get_initial_program_state : program_state = 
  (get_program_table, 0, 0)

let is_program_eof (prog: program) (state: program_state): bool = 
  let (_, _, index) = state in 
    List.length prog = index;;

let rec execute_program (prog: program) (state: program_state): program_state = 
  match is_program_eof prog state with
  | true -> state
  | false -> execute_program prog (advance_statement_index (execute_current_instruction prog state))

let rec read_program_from_stream (stream: in_channel): program = 
  let rec fill_list acc =
    try
      let character = input_char stream in 
        match character with
        | '>' -> fill_list (IncDataPointer::acc)
        | '<' -> fill_list (DecDataPointer::acc)
        | '+' -> fill_list (IncByte::acc)
        | '-' -> fill_list (DecByte::acc)
        | '.' -> fill_list (OutByte::acc)
        | ',' -> fill_list (AccByte::acc)
        | '[' -> fill_list (JmpForward::acc)
        | ']' -> fill_list (JmpBackward::acc)
        | _ -> fill_list acc
    with End_of_file -> List.rev acc
  in fill_list []

let read_program_from_file (filename: string) : program = 
  let stream = open_in filename in
    read_program_from_stream stream;;

let main ():unit = 
  if Array.length Sys.argv = 2 then
    let program_file = Sys.argv.(1) in 
      let program_state = get_initial_program_state in
        let program_example = read_program_from_file program_file in
          ignore((execute_program program_example program_state))
  else print_endline "Usage: brainfuck filename.bf";;

main()