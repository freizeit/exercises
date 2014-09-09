open Core.Std


let rec read_N_lines_ file n blen lines =
  if n = 0 then List.rev lines
  else
    let maybe_line = In_channel.input_line file in
    match maybe_line with
    | None ->
      (* end of input? *)
      if lines = [] then lines
      else (* input cut off in the middle of a n-line block *)
        let msg = "Failed to read " ^ string_of_int blen ^ "-line block" in
        raise (Failure msg)
    | Some line ->
      read_N_lines_ file (n - 1) blen (line :: lines)


let read_N_lines file n =
  if n <= 0 then raise (Failure "block length must be > 0")
  else read_N_lines_ file n n []


let rec process_lines file =
  let block = read_N_lines file 3 in
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
