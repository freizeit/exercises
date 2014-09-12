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
  assert_equal "Case #8: 6 7" result



let suite =
"Input, Logic functions ">:::
  ["happy case: 3-line block">:: test_happy_case;
   "happy case: empty file">:: test_empty_file;
   "failure   : incomplete 4-line block">:: test_incomplete_block;
   "failure   : invalid block length">:: test_invalid_block_length;
   "format_result with None">:: test_format_result_None;
   "format_result with Some">:: test_format_result_Some;
   "indexed_prices with len 2">:: test_indexed_prices_len_2;
   "indexed_prices with len 1">:: test_indexed_prices_len_1;
   "indexed_prices with len 0">:: test_indexed_prices_len_0]


let () =
  run_test_tt_main suite
