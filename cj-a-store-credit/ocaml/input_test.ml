open Core.Std
open OUnit2


let wtf content =
  let path = Filename.temp_file "a" "b" in
  Out_channel.with_file path ~f:(fun file ->
    Out_channel.output_string file content;
    path
  )

let test1 ctxt =
  let path = wtf "a\nb\nc" in
  In_channel.with_file path ~f:(fun file ->
    let block = Input.read_N_lines file 3 [] in
    assert_equal ["a"; "b"; "c"] block
  )


let suite =
"suite">:::
  ["test1">:: test1]


let () =
  run_test_tt_main suite
