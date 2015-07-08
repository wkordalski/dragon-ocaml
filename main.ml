(* Come CLI for Dragon *)

(* Running options *)
let verbose = ref false
let max_files_to_list = ref 42
let dir_to_list = ref "."

let output = ref ""

let files = ref []

(* Main function *)
let main =
begin
  let speclist =
  [
    ("-o", Arg.Set_string output, "Sets output file")
  ]
  in
  let usage_msg = "Dragon compiler utility."
  in
  Arg.parse speclist (fun s -> files := s::!files) usage_msg;
  match !files with
  | [input] ->
  begin
    let infl = open_in input in
    try
      let instr = Stream.of_channel infl in
      (* TODO: lex & parse *)
      let tokstr = Dragon.lex instr in
      let parses = Dragon.parse_stream tokstr in
      let _ = match parses with
      | [progr] ->
      begin
        () (* Parsing OK! *)
      end
      | _ -> print_endline "Error!"
      in
      close_in infl
    with
    | e -> (close_in_noerr infl; raise e)
  end
  | [] -> print_endline "Not enough files to process"
  | _ -> print_endline "Too much files to process"
end

let () = main
