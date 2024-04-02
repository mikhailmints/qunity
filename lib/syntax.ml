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

let rec string_of_type (t : exprtype) =
  match t with
  | _ when t = bit -> "bit"
  | Void -> "Void"
  | Qunit -> "()"
  | SumType (t0, t1) ->
      Printf.sprintf "(%s) + (%s)" (string_of_type t0) (string_of_type t1)
  | ProdType (t0, t1) ->
      Printf.sprintf "(%s) * (%s)" (string_of_type t0) (string_of_type t1)

let rec string_of_expr (e : expr) : string =
  match e with
  | _ when e = bit0 -> "0"
  | _ when e = bit1 -> "1"
  | Null -> "()"
  | Var x -> x
  | Qpair (e1, e2) ->
      Printf.sprintf "(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | Ctrl (e0, t0, l, t1) ->
      Printf.sprintf "Ctrl %s {%s, %s} [%s]" (string_of_expr e0)
        (string_of_type t0) (string_of_type t1)
        (List.fold_left
           (fun s1 s2 -> if s2 = "" then s1 else s1 ^ ", " ^ s2)
           ""
           (List.map
              (fun (ej, ej') ->
                Printf.sprintf "(%s -> %s)" (string_of_expr ej)
                  (string_of_expr ej'))
              l))
  | Try (e0, e1) ->
      Printf.sprintf "try %s catch %s" (string_of_expr e0) (string_of_expr e1)
  | Apply (f, e') ->
      Printf.sprintf "%s (%s)" (string_of_prog f) (string_of_expr e')

and string_of_prog (f : prog) : string =
  match f with
  | _ when f = had -> "had"
  | _ when f = qnot -> "qnot"
  | U3 (theta, phi, lambda) ->
      Printf.sprintf "U3(%s, %s, %s)" (string_of_real theta)
        (string_of_real phi) (string_of_real lambda)
  | Left (t0, t1) ->
      Printf.sprintf "left{%s, %s}" (string_of_type t0) (string_of_type t1)
  | Right (t0, t1) ->
      Printf.sprintf "right{%s, %s}" (string_of_type t0) (string_of_type t1)
  | Lambda (e, t, e') ->
      Printf.sprintf "lambda (%s){%s} -> (%s)" (string_of_expr e)
        (string_of_type t) (string_of_expr e')
  | Gphase (t, r) ->
      Printf.sprintf "gphase{%s}(%s)" (string_of_type t) (string_of_real r)

let string_of_progtype (ft : progtype) : string =
  match ft with
  | Coherent (t0, t1) ->
      Printf.sprintf "%s ~> %s" (string_of_type t0) (string_of_type t1)
  | Channel (t0, t1) ->
      Printf.sprintf "%s ==> %s" (string_of_type t0) (string_of_type t1)
