open Qunity_prototypes
open Util
open Matrix
open Syntax
open Typechecking
open Semantics

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
    Parser.qunityfile Lexer.read lexbuf

let execute (s : string) : unit =
  let e = parse s in
    Printf.printf "==========\nExpression: %s\n" (string_of_expr e);
    match mixed_type_check StringMap.empty e with
    | NoneE err -> Printf.printf "Typechecking error: %s\n" err
    | SomeE t -> begin
        Printf.printf "\nType: %s\n" (string_of_type t);
        Printf.printf "\nPure Semantics:\n";
        begin
          try print_mat (top_pure_expr_semantics e) with
          | Failure _
          | Invalid_argument _ ->
              Printf.printf "None\n"
        end;
        Printf.printf "\nMixed Semantics:\n";
        print_mat (top_mixed_expr_semantics e);
        Printf.printf "\n"
      end

let () =
  begin
    execute "()";
    execute "left {qunit, qunit} ()";
    execute "right {qunit, qunit} ()";
    execute "left {qunit, qunit} () |> u3 {pi/2, 0, pi}";
    execute "right {qunit, qunit} () |> u3 {pi/2, 0, pi}";
    execute
      "left {qunit, qunit} () |> lambda x {qunit + qunit} -> left {qunit, \
       qunit} ()";
    execute
      "let x {qunit + qunit} = left {qunit, qunit} () |> u3 {pi/2, 0, pi} in (\n\
      \      ctrl {qunit + qunit, qunit + qunit} x [\n\
      \        left {qunit, qunit} () -> x,\n\
      \        right {qunit, qunit} () -> x |> gphase {qunit + qunit, pi}\n\
      \      ]\n\
      \    ) |> u3 {pi/2, 0, pi}";
    execute
      "let x {qunit + qunit} = left {qunit, qunit} () |> u3 {pi/2, 0, pi} in (\n\
      \      ctrl {qunit + qunit, qunit + qunit}\n\
      \            (left {qunit, qunit} ()) [\n\
      \        left {qunit, qunit} () -> x,\n\
      \        right {qunit, qunit} () -> x |> gphase {qunit + qunit, pi}\n\
      \      ]\n\
      \    ) |> u3 {pi/2, 0, pi}"
  end
