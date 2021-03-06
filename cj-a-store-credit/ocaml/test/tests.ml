open Core.Std
open OUnit2
open Sys


(* Input function tests *)
(* Writes a temporary file to be used for a test *)
let wtf ctxt content =
  let path, outf = bracket_tmpfile ctxt in
    Out_channel.output_string outf content;
    Out_channel.close outf;
    path


let test_get_blocks_happy_case _ctxt =
  let path = wtf _ctxt "3
    100
    3
    5 75 25
    200
    7
    150 24 79 50 88 345 3
    8
    8
    2 1 9 4 4 56 90 3" in
  let bs = Input.get_blocks path in
  assert_equal [(1, ["100";"3";"5 75 25"]);(2, ["200";"7";"150 24 79 50 88 345 3"]);(3, ["8";"8";"2 1 9 4 4 56 90 3"])] bs


let test_get_blocks_malformed_file _ctxt =
  let path = wtf _ctxt "3
    100
    3" in
  assert_raises (Failure "Malformed input file") (fun _ -> Input.get_blocks path)


(* Logic function tests *)
let test_indexed_prices_len_2 _ctxt =
  let result = Logic.indexed_prices "111 112" in
  assert_equal [(1, 111); (2, 112)] result


let test_indexed_prices_len_1 _ctxt =
  let result = Logic.indexed_prices "111" in
  assert_equal [(1, 111)] result


let test_indexed_prices_len_0 _ctxt =
  assert_raises (Failure "Int.of_string: \"\"")
                (fun _ -> Logic.indexed_prices "")


let test_format_result_None _ctxt =
  let result = Logic.format_result (9, None) in
  assert_equal "Case #9: no solution found" result


let test_format_result_Some _ctxt =
  let result = Logic.format_result (8, Some(6, 7)) in
  assert_equal ~printer:(fun x -> x) "Case #8: 6 7" result


let test_process_block_short _ctxt =
  let lines = ["10"; "3"; "1 7 9"] in
  let result = Logic.process_block (18, lines) in
  assert_equal ~printer:(fun x -> x) "Case #18: 1 3" result


let test_process_block_no_solution _ctxt =
  let lines = ["15"; "4"; "2 1 7 9"] in
  let result = Logic.process_block (19, lines) in
  assert_equal ~printer:(fun x -> x) "Case #19: no solution found" result

let test_process_block_no_lines _ctxt =
  assert_raises (Failure "Block shorter than 3 lines")
                (fun _ -> Logic.process_block ((-1), []))

let test_process_block_1_line _ctxt =
  assert_raises (Failure "Block shorter than 3 lines")
                (fun _ -> Logic.process_block ((-1), ["1"]))

let test_process_block_2_lines _ctxt =
  assert_raises (Failure "Block shorter than 3 lines")
                (fun _ -> Logic.process_block ((-1), ["1"; "2"]))


let suite =
"Input, Logic functions ">:::
  ["process_block with no solution">:: test_process_block_no_solution;
   "process_block with 1 7 9">:: test_process_block_short;
   "process_block with no lines">:: test_process_block_no_lines;
   "process_block with 1 line">:: test_process_block_1_line;
   "process_block with 2 lines">:: test_process_block_2_lines;
   "get_blocks, happy case">:: test_get_blocks_happy_case;
   "get_blocks, malformed input file">:: test_get_blocks_malformed_file;
   "format_result with None">:: test_format_result_None;
   "format_result with Some">:: test_format_result_Some;
   "indexed_prices with len 2">:: test_indexed_prices_len_2;
   "indexed_prices with len 1">:: test_indexed_prices_len_1;
   "indexed_prices with len 0">:: test_indexed_prices_len_0]


let () =
  run_test_tt_main suite
