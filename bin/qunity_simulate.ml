open Driver_util
open Qunity_prototypes
open Util
open Extended_syntax

let execute (s : string) : unit =
  let qunity_stdlib = read_file "bin/stdlib.qunity" in
  let s' = qunity_stdlib ^ "\n" ^ s in
    match
      try Some (parse_file s') with
      | _ -> None
    with
    | None -> Printf.printf "Parse error\n\n"
    | Some qf -> begin
        match preprocess qf with
        | NoneE err -> Printf.printf "Preprocessing error: %s\n\n" err
        | SomeE e -> execute_expr e
      end

let () =
  let prog_filename = Sys.argv.(1) in
  let prog_string = read_file prog_filename in

  execute prog_string
