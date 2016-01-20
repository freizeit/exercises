(** Reads "Store Credit" files in chunks of 3 lines. Each of these blocks constitutes a single "Store Credit" problem. Please @see <https://code.google.com/codejam/contest/dashboard?c=351101#s=p0> the google code jam page for more details *)

(** Reads a file with the given path and returns a list of 2-tuples where the
 * first item is a 1-based ("Store Credit" problem) index and the second is a
 * list with the 3 lines that define a single problem *)
val get_blocks : string -> (int * string list) list
val process_file : string -> int -> int -> unit -> unit
