open Core.Std
open OUnit2
open Sys


let teardown _ =
  Sys.command "rm -f /tmp/ocaml_*_test"

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


let test_incomplete_block _ctxt =
  let path = wtf _ctxt "d\ne\nf" in
  In_channel.with_file path ~f:(fun file ->
    assert_raises (Failure "Failed to read 4-line block")
                  (fun _ -> Input.read_N_lines file 4)
  )


let test_empty_file _ctxt =
  let path = wtf _ctxt "" in
  In_channel.with_file path ~f:(fun file ->
    let block = Input.read_N_lines file 3 in
    assert_equal [] block
  )


let suite =
"read_N_lines ">:::
  ["happy case: reads 3-line block">:: test_happy_case;
   "failure   : incomplete 3-line block">:: test_incomplete_block;
   "happy case: empty file">:: test_happy_case]


let () =
  run_test_tt_main suite
