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
  let num_prices = List.length prices + 1 in
  List.zip (List.range 1 num_prices) prices in

let process_block' lines n =
  let l1 :: _ :: l3 :: [] = lines in
  let credit = Int.of_string l1 in
  let prices = indexed_prices l3 in
  let result = format_result (solve credit prices n) in
  print_endline result


let process_block lines n =
  begin
    List.iter ~f:print_endline lines;
    print_endline "--";
    let l1 :: l2 :: l3 :: [] = lines in
    let credit = Int.of_string l1 in
    let prices = List.map ~f:Int.of_string (String.split ~on:' ' l3) in
    let indexed_prices = List.zip (List.range 1 (l2 + 1)) prices in
    let result = format_result (solve credit indexed_prices) in
    print_endline result
  end


let format_result = function
  | (i1, Some (i2, i3)) -> sprintf "Case #%d: %d %d" i1 i2 i3
  | (i1, None) -> sprintf "Case #%d: no solution found" i1
