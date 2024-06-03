open Qunity_prototypes
open Util
open Matrix
open Syntax
open Extended_syntax
open Typechecking
open Semantics
open Compilation

let read_file (filename : string) : string =
  In_channel.with_open_bin filename In_channel.input_all

let parse_file (s : string) : qunityfile =
  let lexbuf = Lexing.from_string s in
    Parser.qunityfile Lexer.read lexbuf

let parse_interact (s : string) : qunityinteract =
  let lexbuf = Lexing.from_string s in
    Parser.qunityinteract Lexer.read lexbuf

let execute_expr (e : expr) : unit =
  match mixed_type_check StringMap.empty e with
  | NoneE err -> Printf.printf "Typechecking error: %s\n\n" err
  | SomeE t -> begin
      Printf.printf "Expression type: %s\n\n%!" (string_of_type t);
      Printf.printf "Pure semantics:\n%!";
      begin
        try print_mat (top_pure_expr_semantics e) with
        | Failure _
        | Invalid_argument _ ->
            Printf.printf "None\n"
      end;
      Printf.printf "\nMixed semantics:\n%!";
      print_mat (top_mixed_expr_semantics e);
      Printf.printf "\nPossible measurement outcomes:\n%!";
      let meas_outcomes = measurement_outcomes e in
      let _ =
        List.map
          (fun (e', p) ->
            Printf.printf "Probability %f: %s\n" p (string_of_expr e'))
          meas_outcomes
      in
      let error_prob =
        1. -. List.fold_left ( +. ) 0. (List.map snd meas_outcomes)
      in
        if not (float_approx_equal error_prob 0.) then
          Printf.printf "Probability %f: Error\n" error_prob;
        Printf.printf "\n"
    end

let compile_file (prog_filename : string) (out_filename : string)
    (gate_compiler : gate -> int -> int list -> int list -> string)
    (annotate : bool) : unit =
  let prog_string = read_file prog_filename in
  let qunity_stdlib = read_file "bin/stdlib.qunity" in
  let s = qunity_stdlib ^ "\n" ^ prog_string in
    match
      try Some (parse_file s) with
      | _ -> None
    with
    | None -> Printf.printf "Parse error\n\n"
    | Some qf -> begin
        match preprocess qf with
        | NoneE err -> Printf.printf "Preprocessing error: %s\n\n" err
        | SomeE e -> begin
            match mixed_type_check StringMap.empty e with
            | NoneE err -> Printf.printf "Typechecking error: %s\n\n" err
            | SomeE _ -> begin
                let gate, nqubits, out_reg, flag_reg =
                  expr_compile annotate e
                in
                let qasm_str = gate_compiler gate nqubits out_reg flag_reg in
                let out_file = open_out out_filename in
                  Printf.fprintf out_file "%s" qasm_str
              end
          end
      end
