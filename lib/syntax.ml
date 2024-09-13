open Reals
open Util

(** A Qunity expression type. *)
type exprtype =
  | Void
  | Qunit
  | SumType of (exprtype * exprtype)
  | ProdType of (exprtype * exprtype)

(** A Qunity program type. *)
type progtype =
  | Coherent of (exprtype * exprtype)
  | Channel of (exprtype * exprtype)

(** A Qunity expression. *)
type expr =
  | Null
  | Var of string
  | Qpair of (expr * expr)
  | Ctrl of (expr * exprtype * exprtype * (expr * expr) list)
  | Match of (expr * exprtype * exprtype * (expr * expr) list)
  | Try of (expr * expr)
  | Apply of (prog * expr)

(** A Qunity program. *)
and prog =
  | U3 of (real * real * real)
  | Left of (exprtype * exprtype)
  | Right of (exprtype * exprtype)
  | Lambda of (expr * exprtype * expr)
  | Rphase of (exprtype * expr * real * real)
  | Pmatch of (exprtype * exprtype * (expr * expr) list)

type context = exprtype StringMap.t
(** A context, mapping variable names to types. *)

type valuation = expr StringMap.t
(** A valuation, mapping variable names to expressions. *)

let bit = SumType (Qunit, Qunit)
let had = U3 (Div (Pi, Const 2), Const 0, Pi)
let qnot = U3 (Pi, Const 0, Pi)
let bit0 = Apply (Left (Qunit, Qunit), Null)
let bit1 = Apply (Right (Qunit, Qunit), Null)
let bitplus = Apply (had, bit0)
let bitminus = Apply (had, bit1)
let qid (t : exprtype) = Lambda (Var "x", t, Var "x")
let gphase (t : exprtype) (r : real) = Rphase (t, Var "_", r, r)
let phaseflip (t : exprtype) = gphase t Pi
let adjoint (t : exprtype) (f : prog) = Lambda (Apply (f, Var "x"), t, Var "x")

(** String representation of a type in Qunity syntax. *)
let rec string_of_type (t : exprtype) =
  match t with
  | _ when t = bit -> "Bit"
  | Void -> "void"
  | Qunit -> "qunit"
  | SumType (t0, t1) ->
      Printf.sprintf "(%s) + (%s)" (string_of_type t0) (string_of_type t1)
  | ProdType (t0, t1) ->
      Printf.sprintf "(%s) * (%s)" (string_of_type t0) (string_of_type t1)

(** String representation of an expression in Qunity syntax. *)
let rec string_of_expr (e : expr) : string =
  match e with
  | _ when e = bit0 -> "Bit0"
  | _ when e = bit1 -> "Bit1"
  | Null -> "()"
  | Var x -> x
  | Qpair (e1, e2) ->
      Printf.sprintf "(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | Ctrl (e0, t0, t1, l) ->
      Printf.sprintf "ctrl {%s, %s} %s [%s]" (string_of_type t0)
        (string_of_type t1) (string_of_expr e0)
        (List.fold_left
           (fun s1 s2 -> if s1 = "" then s2 else s1 ^ "; " ^ s2)
           ""
           (List.map
              (fun (ej, ej') ->
                Printf.sprintf "(%s) -> (%s)" (string_of_expr ej)
                  (string_of_expr ej'))
              l))
  | Match (e0, t0, t1, l) ->
      Printf.sprintf "match {%s, %s} %s [%s]" (string_of_type t0)
        (string_of_type t1) (string_of_expr e0)
        (List.fold_left
           (fun s1 s2 -> if s1 = "" then s2 else s1 ^ "; " ^ s2)
           ""
           (List.map
              (fun (ej, ej') ->
                Printf.sprintf "(%s) -> (%s)" (string_of_expr ej)
                  (string_of_expr ej'))
              l))
  | Try (e0, e1) ->
      Printf.sprintf "try %s catch %s" (string_of_expr e0) (string_of_expr e1)
  | Apply (f, e') ->
      Printf.sprintf "(%s) of (%s)" (string_of_prog f) (string_of_expr e')

(** String representation of a program in Qunity syntax. *)
and string_of_prog (f : prog) : string =
  match f with
  | _ when f = had -> "Had"
  | _ when f = qnot -> "Qnot"
  | U3 (theta, phi, lambda) ->
      Printf.sprintf "u3{%s, %s, %s}" (string_of_real theta)
        (string_of_real phi) (string_of_real lambda)
  | Left (t0, t1) ->
      Printf.sprintf "left{%s, %s}" (string_of_type t0) (string_of_type t1)
  | Right (t0, t1) ->
      Printf.sprintf "right{%s, %s}" (string_of_type t0) (string_of_type t1)
  | Lambda (e, t, e') ->
      Printf.sprintf "lambda (%s){%s} -> (%s)" (string_of_expr e)
        (string_of_type t) (string_of_expr e')
  | Rphase (t, e, r0, r1) ->
      Printf.sprintf "rphase{%s, %s, %s, %s}" (string_of_type t)
        (string_of_expr e) (string_of_real r0) (string_of_real r1)
  | Pmatch (t0, t1, l) ->
      Printf.sprintf "pmatch {%s, %s} [%s]" (string_of_type t0)
        (string_of_type t1)
        (List.fold_left
           (fun s1 s2 -> if s1 = "" then s2 else s1 ^ "; " ^ s2)
           ""
           (List.map
              (fun (ej, ej') ->
                Printf.sprintf "(%s) -> (%s)" (string_of_expr ej)
                  (string_of_expr ej'))
              l))

(** String representation of a program type. *)
let string_of_progtype (ft : progtype) : string =
  match ft with
  | Coherent (t0, t1) ->
      Printf.sprintf "%s ~> %s" (string_of_type t0) (string_of_type t1)
  | Channel (t0, t1) ->
      Printf.sprintf "%s ==> %s" (string_of_type t0) (string_of_type t1)

(** String representation of a type in OCaml syntax. *)
let rec ocaml_string_of_type (t : exprtype) : string =
  match t with
  | _ when t = bit -> "bit"
  | Void -> "Void"
  | Qunit -> "Qunit"
  | SumType (t0, t1) ->
      Printf.sprintf "SumType (%s, %s)" (ocaml_string_of_type t0)
        (ocaml_string_of_type t1)
  | ProdType (t0, t1) ->
      Printf.sprintf "ProdType (%s, %s)" (ocaml_string_of_type t0)
        (ocaml_string_of_type t1)

(** String representation of an expression in OCaml syntax. *)
and ocaml_string_of_expr (e : expr) : string =
  match e with
  | _ when e = bit0 -> "bit0"
  | _ when e = bit1 -> "bit1"
  | Null -> "Null"
  | Var x -> Printf.sprintf "Var \"%s\"" x
  | Qpair (e0, e1) ->
      Printf.sprintf "Qpair (%s, %s)" (ocaml_string_of_expr e0)
        (ocaml_string_of_expr e1)
  | Ctrl (e0, t0, t1, l) ->
      Printf.sprintf "Ctrl (%s, %s, %s, %s)" (ocaml_string_of_expr e0)
        (ocaml_string_of_type t0) (ocaml_string_of_type t1)
        (string_of_list
           (fun (ej, ej') ->
             Printf.sprintf "(%s, %s)" (ocaml_string_of_expr ej)
               (ocaml_string_of_expr ej'))
           l)
  | Match (e0, t0, t1, l) ->
      Printf.sprintf "Match (%s, %s, %s, %s)" (ocaml_string_of_expr e0)
        (ocaml_string_of_type t0) (ocaml_string_of_type t1)
        (string_of_list
           (fun (ej, ej') ->
             Printf.sprintf "(%s, %s)" (ocaml_string_of_expr ej)
               (ocaml_string_of_expr ej'))
           l)
  | Try (e0, e1) ->
      Printf.sprintf "Try (%s, %s)" (ocaml_string_of_expr e0)
        (ocaml_string_of_expr e1)
  | Apply (f, e') ->
      Printf.sprintf "Apply (%s, %s)" (ocaml_string_of_prog f)
        (ocaml_string_of_expr e')

(** String representation of a program in OCaml syntax. *)
and ocaml_string_of_prog (f : prog) : string =
  match f with
  | _ when f = had -> "had"
  | _ when f = qnot -> "qnot"
  | U3 (theta, phi, lambda) ->
      Printf.sprintf "U3 (%s, %s, %s)"
        (ocaml_string_of_real theta)
        (ocaml_string_of_real phi)
        (ocaml_string_of_real lambda)
  | Left (t0, t1) ->
      Printf.sprintf "Left (%s, %s)" (ocaml_string_of_type t0)
        (ocaml_string_of_type t1)
  | Right (t0, t1) ->
      Printf.sprintf "Right (%s, %s)" (ocaml_string_of_type t0)
        (ocaml_string_of_type t1)
  | Lambda (e, t, e') ->
      Printf.sprintf "Lambda (%s, %s, %s)" (ocaml_string_of_expr e)
        (ocaml_string_of_type t) (ocaml_string_of_expr e')
  | Rphase (t, e, r0, r1) ->
      Printf.sprintf "Rphase (%s, %s, %s, %s)" (ocaml_string_of_type t)
        (ocaml_string_of_expr e) (ocaml_string_of_real r0)
        (ocaml_string_of_real r1)
  | Pmatch (t0, t1, l) ->
      Printf.sprintf "Pmatch (%s, %s, %s)" (ocaml_string_of_type t0)
        (ocaml_string_of_type t1)
        (string_of_list
           (fun (ej, ej') ->
             Printf.sprintf "(%s, %s)" (ocaml_string_of_expr ej)
               (ocaml_string_of_expr ej'))
           l)

(** String representation of a context. *)
let string_of_context (d : context) =
  string_of_list
    (fun (x, e) -> Printf.sprintf "%s : %s" x (string_of_type e))
    (StringMap.bindings d)

(** String representation of a valuation. *)
let string_of_valuation (sigma : valuation) =
  string_of_list
    (fun (x, e) -> Printf.sprintf "%s = %s" x (string_of_expr e))
    (StringMap.bindings sigma)
