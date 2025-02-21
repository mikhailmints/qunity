open Util
open Reals
open Syntax
open Typechecking

(** Comparison operators for the extended Qunity syntax. [Equal] can be applied
    to reals, expressions, types, and programs, while the other operators can
    only be applied to reals. *)
type comparison = Equal | Leq | Lt | Geq | Gt

(** Expressions for the extended Qunity syntax, which are the initial output of
    the parser. This groups together Qunity expressions, types, programs, and
    reals, as well as syntactic sugar ([XMatch], [XPmatch]) and the
    metaprogramming-level constructs of definition invocations ([XInvoke]) and
    if-comparison statements ([XIfcmp]). *)
type xexpr =
  | XNull
  | XVar of string
  | XQpair of (xexpr * xexpr)
  | XCtrl of (xexpr * xexpr * xexpr * (xexpr * xexpr) list * xexpr option)
  | XMatch of (xexpr * xexpr * xexpr * (xexpr * xexpr) list * xexpr option)
  | XPMatch of (xexpr * xexpr * (xexpr * xexpr) list)
  | XTry of (xexpr * xexpr)
  | XApply of (xexpr * xexpr)
  | XVoid
  | XQunit
  | XSumType of (xexpr * xexpr)
  | XProdType of (xexpr * xexpr)
  | XU3 of (xexpr * xexpr * xexpr)
  | XLeft of (xexpr * xexpr)
  | XRight of (xexpr * xexpr)
  | XLambda of (xexpr * xexpr * xexpr)
  | XRphase of (xexpr * xexpr * xexpr * xexpr)
  | XReal of xexpr
  | XInvoke of string * xexpr list
  | XIfcmp of (xexpr * comparison * xexpr * xexpr * xexpr)
  | XPi
  | XEuler
  | XConst of int
  | XRealVar of string
  | XNegate of xexpr
  | XPlus of (xexpr * xexpr)
  | XTimes of (xexpr * xexpr)
  | XDiv of (xexpr * xexpr)
  | XPow of (xexpr * xexpr)
  | XMod of (xexpr * xexpr)
  | XSin of xexpr
  | XCos of xexpr
  | XTan of xexpr
  | XArcsin of xexpr
  | XArccos of xexpr
  | XArctan of xexpr
  | XExp of xexpr
  | XLn of xexpr
  | XLog2 of xexpr
  | XSqrt of xexpr
  | XCeil of xexpr
  | XFloor of xexpr
  | XFail

(** Result of preprocessing an extended Qunity expression, wrapping a Qunity
    real, type, program, or expression, or signal a preprocessing error. *)
and xresult =
  | RReal of real
  | RType of exprtype
  | RProg of prog
  | RExpr of expr
  | RNone of string

and definition = string list * xexpr
(** A definition consisting of argument names and the body. *)

and defmap = definition StringMap.t
(** A map matching names to definitions. *)

and xvaluation = xresult StringMap.t
(** A map matching names to already-evaluated results. *)

type qunityfile = { dm : defmap; main : xexpr option }
(** Structure of a Qunity file output by the parser. *)

(** Converts a real extended-syntax expression into a real. *)
let rec realexpr_eval (r : xexpr) (dm : defmap) (xv : xvaluation) : real =
  match r with
  | XPi -> Pi
  | XEuler -> Euler
  | XConst x -> Const x
  | XRealVar x -> begin
      match StringMap.find_opt x xv with
      | Some value -> begin
          match value with
          | RReal r -> r
          | RNone err -> failwith err
          | _ -> failwith "Expected real"
        end
      | _ -> begin
          match xexpr_eval (XInvoke (x, [])) dm xv with
          | RReal r -> r
          | _ -> failwith (Printf.sprintf "Value %s not found" x)
        end
    end
  | XNegate r0 -> Negate (realexpr_eval r0 dm xv)
  | XPlus (r0, r1) -> Plus (realexpr_eval r0 dm xv, realexpr_eval r1 dm xv)
  | XTimes (r0, r1) -> Times (realexpr_eval r0 dm xv, realexpr_eval r1 dm xv)
  | XDiv (r0, r1) -> Div (realexpr_eval r0 dm xv, realexpr_eval r1 dm xv)
  | XPow (r0, r1) -> Pow (realexpr_eval r0 dm xv, realexpr_eval r1 dm xv)
  | XMod (r0, r1) -> Mod (realexpr_eval r0 dm xv, realexpr_eval r1 dm xv)
  | XSin r0 -> Sin (realexpr_eval r0 dm xv)
  | XCos r0 -> Cos (realexpr_eval r0 dm xv)
  | XTan r0 -> Tan (realexpr_eval r0 dm xv)
  | XArcsin r0 -> Arcsin (realexpr_eval r0 dm xv)
  | XArccos r0 -> Arccos (realexpr_eval r0 dm xv)
  | XArctan r0 -> Arctan (realexpr_eval r0 dm xv)
  | XExp r0 -> Exp (realexpr_eval r0 dm xv)
  | XLn r0 -> Ln (realexpr_eval r0 dm xv)
  | XLog2 r0 -> Log2 (realexpr_eval r0 dm xv)
  | XSqrt r0 -> Sqrt (realexpr_eval r0 dm xv)
  | XCeil r0 -> Ceil (realexpr_eval r0 dm xv)
  | XFloor r0 -> Floor (realexpr_eval r0 dm xv)
  | _ -> failwith "Not a realexpr"

(** Converts an extended-syntax expression into a Qunity expression, type,
    program, or real. *)
and xexpr_eval (v : xexpr) (dm : defmap) (xv : xvaluation) : xresult =
  let expand_xelse (t0 : exprtype) (xelseopt : xexpr option)
      (l : (expr * expr) list) : (expr * expr) list optionE =
    match xelseopt with
    | None -> SomeE l
    | Some xelse -> begin
        match missing_span t0 (List.map fst l) true with
        | None -> NoneE "Ortho check failed when preprocessing else expression"
        | Some (mspan, _) -> begin
            match xexpr_eval xelse dm xv with
            | RNone err -> NoneE (err ^ "\nin Ctrl")
            | RExpr eelse -> SomeE (l @ List.map (fun e -> (e, eelse)) mspan)
            | _ -> NoneE "Expected expression"
          end
      end
  in
  let ctrl_eval (xe0 : xexpr) (xt0 : xexpr) (xt1 : xexpr)
      (xl : (xexpr * xexpr) list) (xelseopt : xexpr option) :
      (expr * exprtype * (expr * expr) list * exprtype) optionE =
    match
      ( xexpr_eval xe0 dm xv,
        xexpr_eval xt0 dm xv,
        all_or_nothing
          (List.map
             (fun (xej, xej') ->
               match (xexpr_eval xej dm xv, xexpr_eval xej' dm xv) with
               | RExpr ej, RExpr ej' -> Some (ej, ej')
               | _ -> None)
             xl),
        xexpr_eval xt1 dm xv )
    with
    | RExpr e0, RType t0, Some l, RType t1 -> begin
        match expand_xelse t0 xelseopt l with
        | NoneE err -> NoneE err
        | SomeE l' -> SomeE (e0, t0, l', t1)
      end
    | RNone err, _, _, _
    | _, RNone err, _, _
    | _, _, _, RNone err ->
        NoneE err
    | _ -> NoneE "Preprocessing error in Ctrl or Match"
  in
    match v with
    | XNull -> RExpr Null
    | XVar x -> RExpr (Var x)
    | XQpair (xe0, xe1) -> begin
        match (xexpr_eval xe0 dm xv, xexpr_eval xe1 dm xv) with
        | RExpr e0, RExpr e1 -> RExpr (Qpair (e0, e1))
        | RNone err, _
        | _, RNone err ->
            RNone (err ^ "\nin Qpair")
        | _, _ -> RNone "Expected expression"
      end
    | XCtrl (xe0, xt0, xt1, xl, xelseopt) -> begin
        match ctrl_eval xe0 xt0 xt1 xl xelseopt with
        | SomeE (e0, t0, l, t1) -> RExpr (Ctrl (e0, t0, t1, l))
        | NoneE err -> RNone err
      end
    | XMatch (xe0, xt0, xt1, xl, xelseopt) -> begin
        match ctrl_eval xe0 xt0 xt1 xl xelseopt with
        | SomeE (e0, t0, l, t1) -> RExpr (Match (e0, t0, t1, l))
        | NoneE err -> RNone err
      end
    | XPMatch (xt0, xt1, xl) -> begin
        match
          ( xexpr_eval xt0 dm xv,
            xexpr_eval xt1 dm xv,
            all_or_nothing
              (List.map
                 (fun (xej, xej') ->
                   match (xexpr_eval xej dm xv, xexpr_eval xej' dm xv) with
                   | RExpr ej, RExpr ej' -> Some (ej, ej')
                   | _ -> None)
                 xl) )
        with
        | RType t0, RType t1, Some l -> RProg (Pmatch (t0, t1, l))
        | _ -> RNone "Expected type in Pmatch"
      end
    | XTry (xe0, xe1) -> begin
        match (xexpr_eval xe0 dm xv, xexpr_eval xe1 dm xv) with
        | RExpr e0, RExpr e1 -> RExpr (Try (e0, e1))
        | RNone err, _
        | _, RNone err ->
            RNone (err ^ "\nin Try")
        | _, _ -> RNone "Expected expression in Try"
      end
    | XApply (xf, xe') -> begin
        match (xexpr_eval xf dm xv, xexpr_eval xe' dm xv) with
        | RProg f, RExpr e' -> RExpr (Apply (f, e'))
        | RNone err, _
        | _, RNone err ->
            RNone (err ^ "\nin Apply")
        | _ -> RNone "Expected expression in Apply"
      end
    | XVoid -> RType Void
    | XQunit -> RType Qunit
    | XSumType (xt0, xt1) -> begin
        match (xexpr_eval xt0 dm xv, xexpr_eval xt1 dm xv) with
        | RType t0, RType t1 -> RType (SumType (t0, t1))
        | RNone err, _
        | _, RNone err ->
            RNone (err ^ "\nin SumType")
        | _ -> RNone "Expected type in SumType"
      end
    | XProdType (xt0, xt1) -> begin
        match (xexpr_eval xt0 dm xv, xexpr_eval xt1 dm xv) with
        | RType t0, RType t1 -> RType (ProdType (t0, t1))
        | RNone err, _
        | _, RNone err ->
            RNone (err ^ "\nin ProdType")
        | _ -> RNone "Expected type in ProdType"
      end
    | XU3 (theta, phi, lambda) -> begin
        try
          RProg
            (U3
               ( realexpr_eval theta dm xv,
                 realexpr_eval phi dm xv,
                 realexpr_eval lambda dm xv ))
        with
        | Failure err -> RNone err
      end
    | XLeft (xt0, xt1) -> begin
        match (xexpr_eval xt0 dm xv, xexpr_eval xt1 dm xv) with
        | RType t0, RType t1 -> RProg (Left (t0, t1))
        | RNone err, _
        | _, RNone err ->
            RNone (err ^ "\nin Left")
        | _ -> RNone "Expected program in Left"
      end
    | XRight (xt0, xt1) -> begin
        match (xexpr_eval xt0 dm xv, xexpr_eval xt1 dm xv) with
        | RType t0, RType t1 -> RProg (Right (t0, t1))
        | RNone err, _
        | _, RNone err ->
            RNone (err ^ "\nin Right")
        | _ -> RNone "Expected program in Right"
      end
    | XLambda (xe, xt, xe') -> begin
        match
          (xexpr_eval xe dm xv, xexpr_eval xt dm xv, xexpr_eval xe' dm xv)
        with
        | RExpr e, RType t, RExpr e' -> RProg (Lambda (e, t, e'))
        | RNone err, _, _
        | _, RNone err, _
        | _, _, RNone err ->
            RNone (err ^ "\nin Lambda")
        | _ -> RNone "Expected program in Lambda"
      end
    | XRphase (xt, xer, r0, r1) -> begin
        match (xexpr_eval xt dm xv, xexpr_eval xer dm xv) with
        | RType t, RExpr er -> begin
            try
              RProg
                (Rphase (t, er, realexpr_eval r0 dm xv, realexpr_eval r1 dm xv))
            with
            | Failure err -> RNone err
          end
        | RNone err, _
        | _, RNone err ->
            RNone (err ^ "\nin Rphase")
        | _ -> RNone "Expected program in Rphase"
      end
    | XReal r -> begin
        try RReal (realexpr_eval r dm xv) with
        | Failure err -> RNone err
      end
    | XInvoke (s, l) -> begin
        match StringMap.find_opt s xv with
        | Some res ->
            if List.length l = 0 then
              res
            else
              RNone "No arguments expected for expression in Invoke"
        | None -> begin
            match StringMap.find_opt s dm with
            | None ->
                RNone (Printf.sprintf "Definition %s not found in Invoke" s)
            | Some (argnames, body) -> begin
                let l_result = List.map (fun x -> xexpr_eval x dm xv) l in
                  if List.length l <> List.length argnames then
                    RNone "Incorrect number of arguments in Invoke"
                  else
                    let xv' =
                      StringMap.of_seq
                        (List.to_seq (List.combine argnames l_result))
                    in
                      xexpr_eval body dm xv'
              end
          end
      end
    | XIfcmp (v0, cmp, v1, vtrue, vfalse) -> begin
        let branch =
          match (xexpr_eval v0 dm xv, cmp, xexpr_eval v1 dm xv) with
          | RReal r0, Equal, RReal r1 -> SomeE (real_equal r0 r1)
          | RReal r0, Leq, RReal r1 -> SomeE (real_le r0 r1)
          | RReal r0, Lt, RReal r1 -> SomeE (real_lt r0 r1)
          | RReal r0, Geq, RReal r1 -> SomeE (real_ge r0 r1)
          | RReal r0, Gt, RReal r1 -> SomeE (real_gt r0 r1)
          | RType t0, Equal, RType t1 -> SomeE (t0 = t1)
          | RProg f0, Equal, RProg f1 -> SomeE (f0 = f1)
          | RExpr e0, Equal, RExpr e1 -> SomeE (e0 = e1)
          | RNone err, _, _
          | _, _, RNone err ->
              NoneE (err ^ "\nin Ifcmp")
          | _ -> NoneE "Inconsistent types in Ifcmp"
        in
          match branch with
          | SomeE true -> xexpr_eval vtrue dm xv
          | SomeE false -> xexpr_eval vfalse dm xv
          | NoneE err -> RNone err
      end
    | XFail -> RNone "Failure triggered"
    | _ -> (
        try RReal (realexpr_eval v dm xv) with
        | Failure err -> RNone err)

(** Adds a definition map [dm_new] to the old [defmap], overriding the old
    values. *)
let add_defmap (dm : defmap) (dm_new : defmap) : defmap =
  StringMap.union (fun _ _ x -> Some x) dm dm_new

(** Adds a new definition [d] with the name [name] to the Qunity file [qf].
    These definitions are added during parsing in reverse order, so newly added
    definitions do not override existing ones. *)
let add_def (name : string) (d : definition) (qf : qunityfile) : qunityfile =
  if StringMap.find_opt name qf.dm <> None then
    qf
  else
    { dm = StringMap.add name d qf.dm; main = qf.main }
