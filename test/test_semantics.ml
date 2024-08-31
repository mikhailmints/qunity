open Qunity
open Matrix
open Syntax
open Semantics

let all_passed = ref true
let c1 = Complex.one
let negc1 = Complex.neg Complex.one
let c0 = Complex.zero
let half : Complex.t = { re = 1. /. 2.; im = 0. }
let sqrthalf : Complex.t = { re = 1. /. Float.sqrt 2.; im = 0. }
let negsqrthalf : Complex.t = { re = -1. /. Float.sqrt 2.; im = 0. }

let test_mat_equality (testname : string) (m1 : unit -> matrix) (m2 : matrix) :
    unit =
  Printf.printf "%s: " testname;
  try
    let m1' = m1 () in
      if mat_approx_equal m1' m2 then
        Printf.printf "passed\n"
      else begin
        Printf.printf "FAILED\n";
        Printf.printf "Expected:\n";
        print_mat m2;
        Printf.printf "Got:\n";
        print_mat m1';
        all_passed := false
      end
  with
  | Failure err
  | Invalid_argument err -> begin
      Printf.printf "FAILED\nWith error: %s\n" err;
      all_passed := false
    end

let expect_pure_expr_sem (testname : string) (e : expr) (l : Complex.t list) :
    unit =
  test_mat_equality testname
    (fun () -> top_pure_expr_semantics e)
    (mat_from_list (List.map (fun x -> [x]) l))

let expect_mixed_expr_sem (testname : string) (e : expr)
    (l : Complex.t list list) : unit =
  test_mat_equality testname
    (fun () -> top_mixed_expr_semantics e)
    (mat_from_list l)

let expect_pure_prog_sem (testname : string) (f : prog)
    (l : Complex.t list list) : unit =
  test_mat_equality testname
    (fun () -> top_pure_prog_semantics f)
    (mat_from_list l)

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
                  bit,
                  [(bit0, Var "x"); (bit1, Apply (phaseflip bit, Var "x"))] )
            ) ),
      Apply (had, bit0) )

let const0 = Lambda (Var "x", bit, bit0)
let const1 = Lambda (Var "x", bit, bit1)

let () =
  begin
    Printf.printf
      "\n\
       =======================\n\
       RUNNING SEMANTICS TESTS\n\
       =======================\n";

    expect_pure_expr_sem "null_sem" Null [c1];

    expect_pure_expr_sem "bit0_sem" bit0 [c1; c0];

    expect_pure_expr_sem "bit1_sem" bit1 [c0; c1];

    expect_mixed_expr_sem "null_mixed_sem" Null [[c1]];

    expect_mixed_expr_sem "bit0_mixed_sem" bit0 [[c1; c0]; [c0; c0]];

    expect_mixed_expr_sem "bit1_mixed_sem" bit1 [[c0; c0]; [c0; c1]];

    expect_pure_expr_sem "had0_sem" (Apply (had, bit0)) [sqrthalf; sqrthalf];

    expect_pure_expr_sem "had1_sem" (Apply (had, bit1)) [sqrthalf; negsqrthalf];

    expect_pure_expr_sem "bit00_sem" (Qpair (bit0, bit0)) [c1; c0; c0; c0];
    expect_pure_expr_sem "bit01_sem" (Qpair (bit0, bit1)) [c0; c1; c0; c0];
    expect_pure_expr_sem "bit10_sem" (Qpair (bit1, bit0)) [c0; c0; c1; c0];
    expect_pure_expr_sem "bit11_sem" (Qpair (bit1, bit1)) [c0; c0; c0; c1];

    expect_pure_prog_sem "qid_qunit_sem" (qid Qunit) [[c1]];

    expect_pure_prog_sem "qid_bit_sem" (qid bit) [[c1; c0]; [c0; c1]];

    expect_pure_expr_sem "half_bell_sem"
      (Ctrl
         ( Apply (had, bit0),
           bit,
           ProdType (bit, bit),
           [(bit0, Qpair (bit0, bit0)); (bit1, Qpair (bit1, bit1))] ))
      [half; c0; c0; half];

    expect_mixed_expr_sem "const0_app_sem"
      (Apply (const0, bit0))
      [[c1; c0]; [c0; c0]];
    expect_mixed_expr_sem "const1_app_sem"
      (Apply (const1, bit0))
      [[c0; c0]; [c0; c1]];
    expect_pure_expr_sem "deutsch_qid_sem" (deutsch (qid bit)) [c0; c1];
    expect_pure_expr_sem "deutsch_qnot_sem" (deutsch qnot) [c0; negc1];
    expect_pure_expr_sem "deutsch_const0_sem" (deutsch const0) [c1; c0];
    expect_pure_expr_sem "deutsch_const1_sem" (deutsch const1) [negc1; c0];

    if !all_passed then
      Printf.printf "\nALL SEMANTICS TESTS PASSED\n\n"
    else begin
      Printf.printf "\nSOME SEMANTICS TESTS FAILED\n\n";
      exit 1
    end
  end
