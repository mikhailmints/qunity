open Qunity
open Util
open Syntax
open Typechecking

let all_passed = ref true

let test_equality (testname : string) (a : 'a) (b : 'a) : unit =
  Printf.printf "%s: " testname;
  if a = b then
    Printf.printf "passed\n"
  else begin
    Printf.printf "FAILED\n";
    all_passed := false
  end

let test_equality_optionE (testname : string) (a : unit -> 'a optionE) (b : 'a)
    (to_string : 'a -> string) : unit =
  Printf.printf "%s: " testname;
  try
    match a () with
    | SomeE b' ->
        if b' = b then
          Printf.printf "passed\n"
        else begin
          Printf.printf "FAILED\nExpected: %s\nGot: %s\n" (to_string b)
            (to_string b');
          all_passed := false
        end
    | NoneE err -> begin
        Printf.printf "FAILED\nWith error: %s\n" err;
        all_passed := false
      end
  with
  | Failure err -> begin
      Printf.printf "FAILED\nWith error: %s\n" err;
      all_passed := false
    end

let expect_noneE (testname : string) (a : unit -> 'a optionE)
    (to_string : 'a -> string) : unit =
  Printf.printf "%s: " testname;
  try
    match a () with
    | SomeE b' -> begin
        Printf.printf "FAILED\nExpected: failure\nGot: %s\n" (to_string b');
        all_passed := false
      end
    | NoneE _ -> Printf.printf "passed\n"
  with
  | Failure err -> begin
      Printf.printf "FAILED\nWith error: %s\n" err;
      all_passed := false
    end

let expect_expr_puretype (testname : string) (e : expr) (t : exprtype) : unit =
  test_equality_optionE testname
    begin
      fun () ->
        match pure_type_check StringMap.empty StringMap.empty e with
        | SomeE tp -> SomeE (type_of_pure_expr_proof tp)
        | NoneE err -> NoneE err
    end
    t string_of_type

let expect_expr_puretype_err (testname : string) (e : expr) : unit =
  expect_noneE testname
    begin
      fun () ->
        match pure_type_check StringMap.empty StringMap.empty e with
        | SomeE tp -> SomeE (type_of_pure_expr_proof tp)
        | NoneE err -> NoneE err
    end
    string_of_type

let expect_expr_mixedtype (testname : string) (e : expr) (t : exprtype) : unit
    =
  test_equality_optionE testname
    begin
      fun () ->
        match mixed_type_check StringMap.empty e with
        | SomeE tp -> SomeE (type_of_mixed_expr_proof tp)
        | NoneE err -> NoneE err
    end
    t string_of_type

(* let expect_expr_mixedtype_err (testname : string) (e : expr) : unit =
   expect_noneE testname
     begin
       fun () ->
         match mixed_type_check StringMap.empty e with
         | SomeE tp -> SomeE (type_of_mixed_expr_proof tp)
         | NoneE err -> NoneE err
     end
     string_of_type *)

let expect_prog_type (testname : string) (f : prog) (ft : progtype) : unit =
  test_equality_optionE testname
    begin
      fun () ->
        match prog_type_check f with
        | SomeE tp -> SomeE (progtype_of_prog_proof tp)
        | NoneE err -> NoneE err
    end
    ft string_of_progtype

(* let expect_prog_type_err (testname : string) (f : prog) : unit =
   expect_noneE testname
     begin
       fun () ->
         match prog_type_check f with
         | SomeE tp -> SomeE (progtype_of_prog_proof tp)
         | NoneE err -> NoneE err
     end
     string_of_progtype *)

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

let big_expr =
  Qpair
    ( Qpair (Apply (Left (ProdType (bit, bit), bit), Qpair (bit0, bit1)), bit0),
      Apply (Right (SumType (bit, bit), bit), bit1) )

let big_expr_type =
  ProdType
    ( ProdType (SumType (ProdType (bit, bit), bit), bit),
      SumType (SumType (bit, bit), bit) )

let () =
  begin
    Printf.printf
      "\n\
       ==========================\n\
       RUNNING TYPECHECKING TESTS\n\
       ==========================\n";

    expect_expr_puretype "qunit_type" Null Qunit;
    expect_expr_puretype "bit0_type" bit0 bit;
    expect_expr_puretype "bit1_type" bit1 bit;

    expect_expr_puretype "big_sum_prod_type" big_expr big_expr_type;
    expect_expr_puretype "bit_pair_type"
      (Qpair (bit0, bit1))
      (ProdType (bit, bit));

    expect_prog_type "qid_unit_type" (qid Qunit) (Coherent (Qunit, Qunit));

    expect_prog_type "qid_bit_type" (qid bit) (Coherent (bit, bit));
    expect_expr_puretype_err "try_pure_err" (Try (Null, Null));
    expect_expr_mixedtype "try_unit_type" (Try (Null, Null)) Qunit;

    test_equality "span_list_qunit1" (span_list Qunit [Null]) (Some [Null]);
    test_equality "span_list_qunit2" (span_list Qunit []) (Some [Null]);
    test_equality "span_list_bit1" (span_list bit []) (Some [Var "$0"]);
    test_equality "span_list_bit2" (span_list bit [bit0]) (Some [bit0; bit1]);
    test_equality "span_list_bit3" (span_list bit [bit1]) (Some [bit1; bit0]);

    test_equality "span_list_2bit"
      (span_list (ProdType (bit, bit)) [Qpair (bit0, bit0)])
      (Some [Qpair (bit0, bit0); Qpair (bit0, bit1); Qpair (bit1, Var "$0")]);

    expect_expr_puretype "bell_type"
      (Ctrl
         ( Apply (had, bit0),
           bit,
           [(bit0, Qpair (bit0, bit0)); (bit1, Qpair (bit1, bit1))],
           ProdType (bit, bit) ))
      (ProdType (bit, bit));

    expect_expr_puretype "deutsch_type" (deutsch (qid bit)) bit;

    expect_expr_puretype_err "deutsch_of_qid_unit_err" (deutsch (qid Qunit));

    expect_expr_puretype_err "deutsch_fail_erase_err"
      (deutsch_fail_erase (qid bit));

    expect_expr_puretype_err "deutsch_fail_ortho_err"
      (deutsch_fail_ortho (qid bit));

    if !all_passed then
      Printf.printf "\nALL TYPECHECKING TESTS PASSED\n\n"
    else
      Printf.printf "\nSOME TYPECHECKING TESTS FAILED\n\n"
  end
