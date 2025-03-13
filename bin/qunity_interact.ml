open Driver_util
open Qunity
open Parsing
open Util
open Syntax
open Metaprogramming

let () =
  let show_ocaml_string = bool_of_string Sys.argv.(1) in
  let stdlib_filename = "qunitylib/stdlib.qunity" in
    match parse_with_err parse_file_qunitylib stdlib_filename with
    | NoneE err -> Printf.printf "%s\n\n" (err ^ " in " ^ stdlib_filename)
    | SomeE stdlib_dm -> begin
        let dm : defmap ref = ref stdlib_dm in
        let curinput = ref "" in
          while true do
            if !curinput = "" then
              Printf.printf "<qunity> ";
            let line = read_line () in
              if line <> "" then
                curinput := !curinput ^ line ^ "\n";
              if String.ends_with ~suffix:";;" line then begin
                curinput := String.sub !curinput 0 (String.length !curinput - 3);
                match
                  ( parse_with_err parse_string_qunitylib !curinput,
                    parse_with_err parse_string_qunityfile !curinput )
                with
                | NoneE err, NoneE _ ->
                    Printf.printf "%s\n\n" err;
                    curinput := ""
                | SomeE dm', _ -> begin
                    curinput := "";
                    dm := combine_defmaps !dm dm'
                  end
                | _, SomeE (dm', xe) -> begin
                    curinput := "";
                    dm := combine_defmaps !dm dm';
                    match xexpr_eval !dm StringMap.empty None xe with
                    | SomeE (e, xt, _) -> begin
                        if show_ocaml_string then
                          Printf.printf "%s\n" (ocaml_string_of_expr e);
                        Printf.printf "xtype: %s\n" (string_of_xtype xt);
                        execute_expr e !dm xt
                      end
                    | NoneE err ->
                        Printf.printf "Preprocessing error: %s\n\n" err
                  end
              end
          done
      end
