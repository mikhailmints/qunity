open Qunity
open Util
open Matrix
open Syntax
open Typechecking
open Semantics
open Metaprogramming

let execute_expr (e : expr) (dm : defmap) (xt : xtype) : unit =
  match mixed_type_check StringMap.empty StringMap.empty e with
  | NoneE err -> Printf.printf "Typechecking error: %s\n\n" err
  | SomeE tp -> begin
      Printf.printf "Expression type: %s\n%!" (string_of_xtype xt);
      Printf.printf "Isometry: %b\n%!" (is_iso_mixed_expr_proof tp);
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
      let meas_probs = measurement_outcomes e in
      let xbasis = all_basis_xexprs dm xt in
      let meas_outcomes =
        List.sort
          (fun (_, p0) (_, p1) ->
            if float_approx_equal p0 p1 then 0 else Stdlib.compare p1 p0)
          ((List.filter (fun (_, p) -> not (float_approx_equal p 0.)))
             (List.combine xbasis meas_probs))
      in
      let _ =
        List.map
          (fun (e', p) ->
            Printf.printf "Probability %f: %s\n" p (string_of_xexpr e'))
          meas_outcomes
      in
      let error_prob =
        1. -. List.fold_left ( +. ) 0. (List.map snd meas_outcomes)
      in
        if not (float_approx_equal error_prob 0.) then
          Printf.printf "Probability %f: Error\n" error_prob;
        Printf.printf "\n"
    end
