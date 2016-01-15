(** Logic needed to process "Store Credit" files. Please @see <https://code.google.com/codejam/contest/dashboard?c=351101#s=p0> the google code jam page for more details *)


open Core.Std


(** Looks for a solution to the "Store record" problem: a store issued a credit
  to a customer and the latter would like to spend all of it in a single
  purchase by finding 2 items so that the sum of their prices exactly matches
  the credit.
   @param credit the credit issued by the store
   @param indexed_prices list of Int 2-tuples where each tuple holds the
    following data: (1-based price index, price)
   @param sri 1-based index of the "Store record" in question
   @return a 2-tuple where the first datum is the index of the store record
    and the second is an Option that is either None if no solution could be
    found or Some (pi1, pi2) where pi1 and pi2 are the indices of the prices
    found.
 *)
let rec solve (credit:int) (indexed_prices:(int * int) list) (sri:int) =
  match indexed_prices with
  | [] -> (sri, None) (* no solution *)
  (* is the price at the head of the list part of the solution ?? *)
  | (hd_i, hd_v) :: t ->
    let result = List.drop_while t ~f:(fun (_, v) -> (v + hd_v) <> credit) in
    match result with
    | [] -> solve credit t sri (* head of list not part of the solution *)
    | (i, _) :: _  -> (sri, Some (hd_i, i)) (* matching price found *)


(** Turns a string with prices into a list of prices (converted to integers).
   @param l3 3rd line of a "Store Credit" record (containg the prices)
   @return list of Int 2-tuples where each tuple holds the following data:
     (1-based price index, price)
 *)
let indexed_prices (l3:string) =
  let prices = List.map ~f:Int.of_string (String.split ~on:' ' l3) in
  List.zip_exn (List.range 1 (List.length prices + 1)) prices


let format_result = function
  | (i1, None) -> sprintf "Case #%d: no solution found" i1
  | (i1, Some (i2, i3)) -> sprintf "Case #%d: %d %d" i1 i2 i3


(** Process a "Store Credit" record and print the result to stdout.
   @param lines 3 lines comprising a "Store Credit" record
   @param sri 1-based store record index (needed for the result string)
 *)
let process_block ((sri:int), (lines:string list)) =
  match lines with
  | [] | [_] | [_; _] -> raise (Failure "Block shorter than 3 lines")
  | l1 :: _ :: l3 :: _ ->
    let credit = Int.of_string l1 in
    let prices = indexed_prices l3 in
    format_result (solve credit prices sri)
