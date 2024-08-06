open Driver_util
open Qunity
open Parsing
open Util
open Reals
open Matrix
open Syntax
open Extended_syntax
open Typechecking
open Semantics

let () =
  let show_ocaml_string = bool_of_string Sys.argv.(1) in
  let stdlib_filename = "qunitylib/stdlib.qunity" in
    match parse_with_err parse_file stdlib_filename with
    | NoneE err -> Printf.printf "%s\n\n" (err ^ "in " ^ stdlib_filename)
    | SomeE stdlib_qf -> begin
        let dm : defmap ref = ref stdlib_qf.dm in
        let curinput = ref "" in
          while true do
            if !curinput = "" then
              Printf.printf "<qunity> ";
            let line = read_line () in
              if line <> "" then
                curinput := !curinput ^ line ^ "\n";
              if String.ends_with ~suffix:";;" line then begin
                match parse_with_err parse_string !curinput with
                | NoneE err ->
                    Printf.printf "%s\n\n" err;
                    curinput := ""
                | SomeE qi -> begin
                    curinput := "";
                    dm := add_defmap !dm qi.dm;
                    match qi.main with
                    | None -> ()
                    | Some xe -> begin
                        match xexpr_eval xe !dm StringMap.empty with
                        | RNone err ->
                            Printf.printf "Preprocessing error: %s\n\n" err
                        | RExpr e -> begin
                            if show_ocaml_string then
                              Printf.printf "%s\n" (ocaml_string_of_expr e);
                            execute_expr e
                          end
                        | RType t -> begin
                            if show_ocaml_string then
                              Printf.printf "%s\n" (ocaml_string_of_type t);
                            Printf.printf "Type: %s\n\n" (string_of_type t)
                          end
                        | RProg f -> begin
                            if show_ocaml_string then
                              Printf.printf "%s\n" (ocaml_string_of_prog f);
                            match prog_type_check f with
                            | SomeE tp ->
                                Printf.printf "Program type: %s\n"
                                  (string_of_progtype
                                     (progtype_of_prog_proof tp));
                                Printf.printf "Isometry: %b\n\n"
                                  (is_iso_prog_proof tp);
                                Printf.printf "Pure semantics:\n%!";
                                begin
                                  try
                                    print_mat (top_pure_prog_semantics f)
                                  with
                                  | Failure _
                                  | Invalid_argument _ ->
                                      Printf.printf "None\n"
                                end;
                                Printf.printf "\nMixed semantics:\n%!";
                                print_superop (top_mixed_prog_semantics f);
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
      end
