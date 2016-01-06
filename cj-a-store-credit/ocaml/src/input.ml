(** Logic needed to read "Store Credit" files. Please @see <https://code.google.com/codejam/contest/dashboard?c=351101#s=p0> the google code jam page for more details *)

open Core.Std


(** Reads a block of lines from a "Store Credit" file
   @param file an open channel to read from
   @param n remaining number of lines to read
   @param total total number of lines to read
   @param lines list collecting the lines read so far
   @return a list of strings where each element is a line read from the file
   @raise Failure if a complete block of lines could not be read
 *)
let rec read_N_lines' file n total lines =
  if n = 0 then List.rev lines
  else
    let maybe_line = In_channel.input_line file in
    match maybe_line with
    | None ->
      (* end of input? *)
      if lines = [] then lines
      else (* input cut off in the middle of a n-line block *)
        let msg = "Failed to read " ^ string_of_int total ^ "-line block" in
        raise (Failure msg)
    | Some line ->
      read_N_lines' file (n - 1) total (line :: lines)


(** Reads a block of lines from a "Store Credit" file
   @see "read_N_lines'" for more detail
   @param file an open channel to read from
   @param n remaining number of lines to read
   @return list of strings where each element is a line read from the file
   @raise Failure if the block size is zero or less
 *)
let read_N_lines file n =
  if n <= 0 then raise (Failure "block length must be > 0")
  else read_N_lines' file n n []


(** Processes all records in a "Store Credit" file while keeping track of the
  1-based index of the records processed.
   @param file an open channel to read from
   @param n 1-based index of the store credit record to process
 *)
let rec process_lines' file n =
  let block = read_N_lines file 3 in
  if block = [] then
    ()
  else begin
    print_endline (Logic.process_block block n);
    (* Keep going until we consume all of the input file *)
    process_lines' file (n + 1)
  end


(** Processes all records in a "Store Credit" file
   @param file an open channel to read from
 *)
let process_lines file = process_lines' file 1


(** Open a "Store Credit" file and make sure all records are processed
   @param path path of the "Store Credit" file to be processed
 *)
let process_file path () =
  In_channel.with_file path ~f:(fun file ->
    (* ignore first line *)
    let _ = In_channel.input_line file in
    process_lines file
  )
