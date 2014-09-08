open Core.Std


let rec read_N_lines file n lines =
  if n = 0 then List.rev lines
  else
    let maybe_line = In_channel.input_line file in
    match maybe_line with
    | None ->
      (* end of input? *)
      if lines = [] then lines
      else (* input cut off in the middle of a 3-line block *)
        raise (Failure "Malformed input, failed to read block of 3 lines")
    | Some line ->
      read_N_lines file (n - 1) (line :: lines)


let rec process_lines file =
  let block = read_N_lines file 3 [] in
  if block = [] then
    ()
  else begin
    Logic.process_block block;
    (* Keep going until we consume all of the input file *)
    process_lines file
  end


let process_file path () =
  In_channel.with_file path ~f:(fun file ->
    (* ignore first line *)
    let _ = In_channel.input_line file in
    process_lines file
  )


let command =
  Command.basic
    ~summary:"Solves the 'store credit' code jam practice problem"
    ~readme:(fun () -> "More detailed information")
    Command.Spec.(empty +> anon ("filename" %: string))
    process_file


let () =
  Command.run ~version:"0.1" ~build_info:"RWO" command
