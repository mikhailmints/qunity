open Qunity_prototypes
open Syntax
open Typechecking

let expect_expr_puretype (e : expr) (t : exprtype) : bool =
  pure_type_check StringMap.empty StringMap.empty e = SomeE t

let expect_expr_puretype_err (e : expr) : bool =
  option_of_optionE (pure_type_check StringMap.empty StringMap.empty e) = None

let expect_expr_mixedtype (e : expr) (t : exprtype) : bool =
  mixed_type_check StringMap.empty e = SomeE t

let expect_expr_mixedtype_err (e : expr) : bool =
  option_of_optionE (mixed_type_check StringMap.empty e) = None

let expect_prog_type (f : prog) (t : progtype) : bool =
  prog_type_check f = SomeE t

let expect_prog_type_err (f : prog) : bool =
  option_of_optionE (prog_type_check f) = None

let deutsch (f : prog) : expr =
  Apply
    ( Lambda
        ( Var "x",
          bit,
          Apply
            ( had,
              Ctrl
                ( Apply (f, Var "x"),
                  bit,
                  [(bit0, Var "x"); (bit1, Apply (phaseflip bit, Var "x"))],
                  bit ) ) ),
      Apply (had, bit0) )

let deutsch_fail_erase (f : prog) : expr =
  Apply
    ( Lambda
        ( Var "x",
          bit,
          Apply
            ( had,
              Ctrl
                ( Apply (f, Var "x"),
                  bit,
                  [(bit0, bit0); (bit1, Apply (phaseflip bit, Var "x"))],
                  bit ) ) ),
      Apply (had, bit0) )

let deutsch_fail_ortho (f : prog) : expr =
  Apply
    ( Lambda
        ( Var "x",
          bit,
          Apply
            ( had,
              Ctrl
                ( Apply (f, Var "x"),
                  bit,
                  [(bit0, Var "x"); (bit0, Apply (phaseflip bit, Var "x"))],
                  bit ) ) ),
      Apply (had, bit0) )

let%test "qunit_type" = expect_expr_puretype Null Qunit
let%test "bit0_type" = expect_expr_puretype bit0 bit
let%test "bit1_type" = expect_expr_puretype bit1 bit

let big_expr =
  Qpair
    ( Qpair (Apply (Left (ProdType (bit, bit), bit), Qpair (bit0, bit1)), bit0),
      Apply (Right (SumType (bit, bit), bit), bit1) )

let big_expr_type =
  ProdType
    ( ProdType (SumType (ProdType (bit, bit), bit), bit),
      SumType (SumType (bit, bit), bit) )

let%test "big_sum_prod_type" = expect_expr_puretype big_expr big_expr_type

let%test "bit_pair_type" =
  expect_expr_puretype (Qpair (bit0, bit1)) (ProdType (bit, bit))

let%test "qid_unit_type" =
  expect_prog_type (qid Qunit) (Coherent (Qunit, Qunit))

let%test "qid_bit_type" = expect_prog_type (qid bit) (Coherent (bit, bit))
let%test "try_pure_err" = expect_expr_puretype_err (Try (Null, Null))
let%test "try_unit_type" = expect_expr_mixedtype (Try (Null, Null)) Qunit
let%test "span_list_qunit1" = span_list Qunit [Null] = Some [Null]
let%test "span_list_qunit2" = span_list Qunit [] = Some [Null]
let%test "span_list_bit1" = span_list bit [] = Some [Var "$0"]
let%test "span_list_bit2" = span_list bit [bit0] = Some [bit0; bit1]
let%test "span_list_bit3" = span_list bit [bit1] = Some [bit1; bit0]

let%test "span_list_2bit" =
  span_list (ProdType (bit, bit)) [Qpair (bit0, bit0)]
  = Some [Qpair (bit0, bit0); Qpair (bit1, Var "$0"); Qpair (bit0, bit1)]

let%test "deutsch_type" =
  expect_expr_puretype (deutsch (qid bit)) (SumType (Qunit, Qunit))

let%test "deutsch_of_qid_unit_err" =
  expect_expr_puretype_err (deutsch (qid Qunit))

let%test "deutsch_fail_erase_err" =
  expect_expr_puretype_err (deutsch_fail_erase (qid bit))

let%test "deutsch_fail_ortho_err" =
  expect_expr_puretype_err (deutsch_fail_ortho (qid bit))

let () = Printf.printf "ALL TESTS PASSED"
