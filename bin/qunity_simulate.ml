open Driver_util
open Qunity
open Util
open Parsing

let () =
  let prog_filename = Sys.argv.(1) in

  match get_expr_from_file prog_filename with
  | NoneE err ->
      Printf.printf "%s\n" err;
      exit 1
  | SomeE (e, dm, xt) -> execute_expr e dm xt
