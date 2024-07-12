open Driver_util
open Qunity
open Util

let () =
  let prog_filename = Sys.argv.(1) in

  match get_expr_from_file prog_filename with
  | NoneE err -> Printf.printf "%s\n" err
  | SomeE e -> execute_expr e
