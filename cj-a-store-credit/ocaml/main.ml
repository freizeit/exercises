open Core.Std


let command =
  Command.basic
    ~summary:"Solves the 'store credit' code jam practice problem"
    ~readme:(fun () -> "More detailed information")
    Command.Spec.(empty +> anon ("filename" %: string))
    Input.process_file


let () =
  Command.run ~version:"0.1" ~build_info:"RWO" command
