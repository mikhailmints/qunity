open Simulate_util
open Qunity_prototypes
open Util
open Extended_syntax

let read_file (filename : string) : string =
  In_channel.with_open_bin filename In_channel.input_all

let parse (s : string) : qunityfile =
  let lexbuf = Lexing.from_string s in
    Parser.qunityfile Lexer.read lexbuf

let execute (s : string) : unit =
  let qunity_stdlib = read_file "bin/stdlib.qunity" in
  let s' = qunity_stdlib ^ "\n" ^ s in
  let qf = parse s' in
    match preprocess qf with
    | NoneE err -> Printf.printf "Preprocessing error: %s\n\n" err
    | SomeE e -> execute_expr e

let () =
  let prog_filename = Sys.argv.(1) in
  let prog_string = read_file prog_filename in

  execute prog_string
