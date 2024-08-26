open Qunity
open Util
open Syntax
open Gate
open Compilation

module Alcotest = struct
  include Alcotest

  let optionE e =
    testable
      (fun ppf x ->
        match x with
        | SomeE v -> (pp e) ppf v
        | NoneE err -> Fmt.string ppf ("NoneE " ^ err))
      (fun a b ->
        match (a, b) with
        | NoneE err_a, NoneE err_b -> err_a = err_b
        | SomeE ea, SomeE eb -> (equal e) ea eb
        | _ -> false)

  let expr =
    testable
      (fun (ppf : Format.formatter) (e : expr) ->
        Fmt.string ppf (string_of_expr e))
      ( = )

  let exprtype =
    testable
      (fun (ppf : Format.formatter) (t : exprtype) ->
        Fmt.string ppf (string_of_type t))
      ( = )

  let prog =
    testable
      (fun (ppf : Format.formatter) (f : prog) ->
        Fmt.string ppf (string_of_prog f))
      ( = )

  let string_map e =
    testable
      (Fmt.Dump.iter_bindings StringMap.iter (Fmt.any "map") Fmt.string (pp e))
      (StringMap.equal ( = ))

  let int_map e =
    testable
      (Fmt.Dump.iter_bindings IntMap.iter (Fmt.any "map") Fmt.int (pp e))
      (IntMap.equal ( = ))

  let string_set =
    testable
      (Fmt.Dump.iter StringSet.iter (Fmt.any "set") Fmt.string)
      StringSet.equal

  let int_set =
    testable (Fmt.Dump.iter IntSet.iter (Fmt.any "set") Fmt.int) IntSet.equal

  let context = string_map exprtype
  let valuation = string_map expr

  let binary_tree =
    testable
      (fun (ppf : Format.formatter) (tree : binary_tree) ->
        Fmt.string ppf (string_of_tree tree))
      ( = )

  let gate =
    testable
      (fun (ppf : Format.formatter) (u : gate) ->
        Fmt.string ppf (string_of_gate (u |> gate_remove_identities)))
      (fun (u0 : gate) (u1 : gate) ->
        u0 |> gate_remove_identities = (u1 |> gate_remove_identities))

  let circuit =
    testable
      (fun (ppf : Format.formatter) (circ : circuit) ->
        Fmt.string ppf
          (string_of_circuit
             { circ with gate = circ.gate |> gate_remove_identities }))
      (fun (circ0 : circuit) (circ1 : circuit) ->
        { circ0 with gate = circ0.gate |> gate_remove_identities }
        = { circ1 with gate = circ1.gate |> gate_remove_identities })
end

let test title f = Alcotest.test_case title `Quick f
