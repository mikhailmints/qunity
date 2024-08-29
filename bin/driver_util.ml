open Qunity
open Util
open Matrix
open Syntax
open Typechecking
open Semantics

let execute_expr (e : expr) : unit =
  match mixed_type_check StringMap.empty e with
  | NoneE err -> Printf.printf "Typechecking error: %s\n\n" err
  | SomeE tp -> begin
      Printf.printf "Expression type: %s\n%!"
        (string_of_type (type_of_mixed_expr_proof tp));
      Printf.printf "Isometry: %b\n%!" (is_iso_mixed_expr_proof tp);
      begin
        match tp with
        | TMix tp' ->
            Printf.printf "Unitary: %b\n\n%!" (is_un_pure_expr_proof tp')
        | _ -> ()
      end;
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
