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


let test_happy_case _ctxt =
  let path = wtf _ctxt "a\nb\nc" in
  In_channel.with_file path ~f:(fun file ->
    let block = Input.read_N_lines file 3 in
    assert_equal ["a"; "b"; "c"] block
  )


let test_empty_file _ctxt =
  let path = wtf _ctxt "" in
  In_channel.with_file path ~f:(fun file ->
    let block = Input.read_N_lines file 3 in
    assert_equal [] block
  )


let test_incomplete_block _ctxt =
  let path = wtf _ctxt "d\ne\nf" in
  In_channel.with_file path ~f:(fun file ->
    assert_raises (Failure "Failed to read 4-line block")
                  (fun _ -> Input.read_N_lines file 4)
  )


let test_invalid_block_length _ctxt =
  let path = wtf _ctxt "g\ne\nf" in
  In_channel.with_file path ~f:(fun file ->
    assert_raises (Failure "block length must be > 0")
                  (fun _ -> Input.read_N_lines file (-5))
  )


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
  let result = Logic.process_block lines 18 in
  assert_equal ~printer:(fun x -> x) "Case #18: 1 3" result


let test_process_block_no_solution _ctxt =
  let lines = ["15"; "4"; "2 1 7 9"] in
  let result = Logic.process_block lines 19 in
  assert_equal ~printer:(fun x -> x) "Case #19: no solution found" result

let test_process_block_no_lines _ctxt =
  assert_raises (Failure "Block shorter than 3 lines")
                (fun _ -> Logic.process_block [] (-1))

let test_process_block_1_line _ctxt =
  assert_raises (Failure "Block shorter than 3 lines")
                (fun _ -> Logic.process_block ["1"] (-1))

let test_process_block_2_lines _ctxt =
  assert_raises (Failure "Block shorter than 3 lines")
                (fun _ -> Logic.process_block ["1"; "2"] (-1))


let suite =
"Input, Logic functions ">:::
  ["happy case: 3-line block">:: test_happy_case;
   "happy case: empty file">:: test_empty_file;
   "failure: incomplete 4-line block">:: test_incomplete_block;
   "failure: invalid block length">:: test_invalid_block_length;
   "process_block with no solution">:: test_process_block_no_solution;
   "process_block with 1 7 9">:: test_process_block_short;
   "process_block with no lines">:: test_process_block_no_lines;
   "process_block with 1 line">:: test_process_block_1_line;
   "process_block with 2 lines">:: test_process_block_2_lines;
   "format_result with None">:: test_format_result_None;
   "format_result with Some">:: test_format_result_Some;
   "indexed_prices with len 2">:: test_indexed_prices_len_2;
   "indexed_prices with len 1">:: test_indexed_prices_len_1;
   "indexed_prices with len 0">:: test_indexed_prices_len_0]


let () =
  run_test_tt_main suite
