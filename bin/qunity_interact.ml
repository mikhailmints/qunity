open Simulate_util
open Qunity_prototypes
open Util
open Reals
open Syntax
open Extended_syntax
open Typechecking

let parse (s : string) : qunityinteract =
  let lexbuf = Lexing.from_string s in
    Parser.qunityinteract Lexer.read lexbuf

let qunity_stdlib = read_file "bin/stdlib.qunity"
let qi = parse (qunity_stdlib ^ "\n;;")
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
          try Some (parse !curinput) with
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
                          (string_of_progtype ft)
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
