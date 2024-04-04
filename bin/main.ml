open Qunity_prototypes
open Util
open Matrix
open Syntax
open Extended_syntax
open Typechecking
open Semantics

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
    | SomeE e -> begin
        Printf.printf "Expression: %s\n%!" (string_of_expr e);
        match mixed_type_check StringMap.empty e with
        | NoneE err -> Printf.printf "Typechecking error: %s\n\n" err
        | SomeE t -> begin
            Printf.printf "\nType: %s\n%!" (string_of_type t);
            Printf.printf "\nPure Semantics:\n%!";
            begin
              try print_mat (top_pure_expr_semantics e) with
              | Failure _
              | Invalid_argument _ ->
                  Printf.printf "None\n"
            end;
            Printf.printf "\nMixed Semantics:\n%!";
            print_mat (top_mixed_expr_semantics e);
            Printf.printf "\n"
          end
      end

let () =
  let prog_filename = Sys.argv.(1) in
  let prog_string = read_file prog_filename in

  execute prog_string
