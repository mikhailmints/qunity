open Qunity
open Util
open Syntax

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
end

let test title f = Alcotest.test_case title `Quick f
