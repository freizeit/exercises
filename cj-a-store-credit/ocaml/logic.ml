open Core.Std


let rec solve credit indexed_prices n =
  match indexed_prices with
  | [] -> (n, None)
  | ihp :: rest ->
    let (fidx, hp) = ihp in
    let result = List.drop_while rest ~f:(fun (_, p) -> (p + hp) <> credit) in
    match result with
    | [] -> solve credit rest n (* no results *)
    | (* matching price found *)
      (sidx, _) :: _  -> (n, Some (fidx, sidx))


let indexed_prices l3 =
  let prices = List.map ~f:Int.of_string (String.split ~on:' ' l3) in
  let num_prices = (List.length prices) + 1 in
  List.zip_exn (List.range 1 num_prices) prices


let format_result = function
  | (i1, None) -> sprintf "Case #%d: no solution found" i1
  | (i1, Some (i2, i3)) -> sprintf "Case #%d: %d %d" i1 i2 i3


let process_block' lines n =
  let l1 :: _ :: l3 :: [] = lines in
  let credit = Int.of_string l1 in
  let prices = indexed_prices l3 in
  let result = solve credit prices n in
  format_result result


let process_block lines n =
  begin
    print_endline (process_block' lines n)
  end
