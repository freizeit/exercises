open Core.Std


let process_block lines =
  begin
    List.iter ~f:print_endline lines;
    print_endline "--"
  end
