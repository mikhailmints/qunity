open Reals

type exprtype =
  | Void
  | Qunit
  | SumType of (exprtype * exprtype)
  | ProdType of (exprtype * exprtype)

type progtype =
  | Coherent of (exprtype * exprtype)
  | Channel of (exprtype * exprtype)

type expr =
  | Null
  | Var of string
  | Qpair of (expr * expr)
  | Ctrl of (expr * exprtype * (expr * expr) list * exprtype)
  | Try of (expr * expr)
  | Apply of (prog * expr)

and prog =
  | U3 of (real * real * real)
  | Left of (exprtype * exprtype)
  | Right of (exprtype * exprtype)
  | Lambda of (expr * exprtype * expr)
  | Gphase of (exprtype * real)

let bit = SumType (Qunit, Qunit)
let had = U3 (Div (Pi, Const 2), Const 0, Pi)
let qnot = U3 (Pi, Const 0, Pi)
let bit0 = Apply (Left (Qunit, Qunit), Null)
let bit1 = Apply (Right (Qunit, Qunit), Null)
let qid (t : exprtype) = Lambda (Var "x", t, Var "x")
let phaseflip (t : exprtype) = Gphase (t, Pi)
let adjoint (t : exprtype) (f : prog) = Lambda (Apply (f, Var "x"), t, Var "x")
