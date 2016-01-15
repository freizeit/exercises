(** Logic needed to process "Store Credit" files. Please @see <https://code.google.com/codejam/contest/dashboard?c=351101#s=p0> the google code jam page for more details *)

val solve : int -> (int * int) list -> int -> int * (int * int) option

(** Takes a string containing store item prices and returns a 2-tuple for each
 * price where the first item is a 1-based (price) index and the second item
 * is the integer price value. The 2-tuples are returned in a list. *)
val indexed_prices : string -> (int * Core.Std.Int.t) Core.Std.List.t

val format_result : int * (int * int) option -> string

(** Takes a 1-based index and a 3-line block (comprising a "store credit"
 * problem) and returns a string with the solution. *)
val process_block : int * string list -> string
