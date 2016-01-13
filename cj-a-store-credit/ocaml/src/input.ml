(** Logic needed to read "Store Credit" files. Please @see <https://code.google.com/codejam/contest/dashboard?c=351101#s=p0> the google code jam page for more details *)

open Core.Std


(** Open a "Store Credit" file and write 2-tuples to the `writer` where the
   first tuple element is a 1-based index and the second is a string (a block
   of 3 lines (that defines a single "Store Credit" problem)).
   @param path path of the "Store Credit" file to be processed
 *)
let get_blocks path =
  let lines = List.drop (In_channel.read_lines path) 1 in
  if ((List.length lines) % 3 <> 0) then
    failwith "Malformed input file"
  else
    let rec loop idx ls =
      let (hd, tl) = List.split_n ls 3 in
      match tl with
        | [] -> [(idx, hd)]
        | _ -> (idx, hd) :: loop (idx+1) tl
    in let tls = List.map lines ~f:String.strip
    in loop 1 (List.map lines ~f:String.strip)


(** Open a "Store Credit" file and make sure all records are processed
   @param path path of the "Store Credit" file to be processed
 *)
let process_file path () =
  get_blocks path
  |> List.iter ~f:(fun b -> print_endline (Logic.process_block b))
