(*
	A program solving

		http://code.google.com/codejam/contest/dashboard?c=351101#s=p0

  A store issues a credit to a customer and the latter would like to spend
  all of it in a single purchase by finding 2 store items so that the sum of
  their prices exactly matches the credit.

	The test cases are contained in a file whose path is to be specified on
	the command line.

	Example:

		./pmain.native ../A-large-practice.in

	This will process the test cases contained in the '../A-large-practice.in'
	file.
*)


open Core.Std
open Async.Std
open Rpc_parallel.Std

module Store_credit_map_function = Map_reduce.Make_map_function(struct
  module Input = struct
    type t = int * string list with bin_io
  end
  module Output = struct
    type t = string with bin_io
  end

  let map b = return (Logic.process_block b)
end)

let command =
  Command.async ~summary:"Solves the 'store credit' code jam practice problem in parallel"
    Command.Spec.(
      empty
      +> flag "path" (required string) ~doc:" Path to the data file"
      +> flag "nworkers" (optional_with_default 4 int) ~doc:" Number of workers"
    )
    (fun path nworkers () ->
       let blocks = (Pipe.of_list (Input.get_blocks path)) in
       (Map_reduce.map
          (Map_reduce.Config.create ~local:nworkers ())
          blocks
          ~m:(module Store_credit_map_function)
          ~param:()
        >>= fun output_reader ->
        Pipe.iter output_reader ~f:(fun solution ->
          print_endline solution;
          Deferred.unit
        ))
    )

let () = Parallel.start_app command
