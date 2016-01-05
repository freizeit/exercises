(*
	A program solving

		http://code.google.com/codejam/contest/dashboard?c=351101#s=p0

  A store issues a credit to a customer and the latter would like to spend
  all of it in a single purchase by finding 2 store items so that the sum of
  their prices exactly matches the credit.

	The test cases are contained in a file whose path is to be specified on
	the command line.

	Example:

		./main.native ../A-large-practice.in

	This will process the test cases contained in the '../A-large-practice.in'
	file.
*)


open Core.Std


let command =
  Command.basic
    ~summary:"Solves the 'store credit' code jam practice problem"
    ~readme:(fun () -> "More detailed information")
    Command.Spec.(empty +> anon ("filename" %: string))
    Input.process_file


let () =
  Command.run ~version:"0.1" ~build_info:"RWO" command
