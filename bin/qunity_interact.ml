open Driver_util
open Qunity_prototypes
open Util
open Reals
open Matrix
open Syntax
open Extended_syntax
open Typechecking
open Semantics

let qunity_stdlib = read_file "bin/stdlib.qunity"
let qi = parse_interact (qunity_stdlib ^ "\n;;")
let dm : defmap ref = ref qi.dm
let curinput = ref ""

let () =
  while true do
    if !curinput = "" then
      Printf.printf "<qunity> ";
    let line = read_line () in
      if line <> "" then
        curinput := !curinput ^ line ^ "\n";
      if String.ends_with ~suffix:";;" line then begin
        match
          try Some (parse_interact !curinput) with
          | _ -> None
        with
        | None -> begin
            Printf.printf "Parse error\n\n";
            curinput := ""
          end
        | Some qi -> begin
            curinput := "";
            dm := add_defmap !dm qi.dm;
            match qi.main with
            | None -> ()
            | Some xe -> begin
                match xexpr_eval xe !dm StringMap.empty with
                | RNone err -> Printf.printf "Preprocessing error: %s\n\n" err
                | RExpr e -> execute_expr e
                | RType t -> Printf.printf "Type: %s\n\n" (string_of_type t)
                | RProg f -> begin
                    match prog_type_check f with
                    | SomeE ft ->
                        Printf.printf "Program type: %s\n\n"
                          (string_of_progtype ft);
                        Printf.printf "Pure semantics:\n%!";
                        begin
                          try print_mat (pure_prog_semantics f) with
                          | Failure _
                          | Invalid_argument _ ->
                              Printf.printf "None\n"
                        end;
                        Printf.printf "\nMixed semantics:\n%!";
                        print_superop (mixed_prog_semantics f);
                        Printf.printf "\n"
                    | NoneE err ->
                        Printf.printf "Typechecking error: %s\n\n" err
                  end
                | RReal r ->
                    Printf.printf "Real: %s\nEvaluates to: %f\n\n"
                      (string_of_real r) (float_of_real r)
              end
          end
      end
  done
