open Util
open Reals
open Matrix
open Syntax
open Typechecking

type valuation = expr StringMap.t

let rec type_dimension (t : exprtype) : int =
  match t with
  | Void -> 0
  | Qunit -> 1
  | SumType (t0, t1) -> type_dimension t0 + type_dimension t1
  | ProdType (t0, t1) -> type_dimension t0 * type_dimension t1

let context_dimension (d : context) : int =
  List.fold_left ( * ) 1
    (List.map (fun (_, t) -> type_dimension t) (StringMap.bindings d))

let rec expr_to_basis_state (e : expr) : matrix =
  let t =
    match pure_type_check StringMap.empty StringMap.empty e with
    | SomeE t -> t
    | NoneE err -> failwith err
  in
    match (e, t) with
    | Null, Qunit -> mat_identity 1
    | Apply (Left (t0, t1), e0), SumType (t0', t1') when t0 = t0' && t1 = t1'
      ->
        vec_dirsum (expr_to_basis_state e0) (vec_zero (type_dimension t1))
    | Apply (Right (t0, t1), e1), SumType (t0', t1') when t0 = t0' && t1 = t1'
      ->
        vec_dirsum (vec_zero (type_dimension t0)) (expr_to_basis_state e1)
    | Qpair (e1, e2), ProdType (_, _) ->
        mat_tensor (expr_to_basis_state e1) (expr_to_basis_state e2)
    | _, _ -> failwith "Type mismatch"

let rec all_basis_exprs (t : exprtype) : expr list =
  match t with
  | Void -> []
  | Qunit -> [Null]
  | SumType (t0, t1) ->
      List.map (fun e0 -> Apply (Left (t0, t1), e0)) (all_basis_exprs t0)
      @ List.map (fun e1 -> Apply (Right (t0, t1), e1)) (all_basis_exprs t1)
  | ProdType (t0, t1) ->
      List.flatten
        (List.map
           (fun e0 -> List.map (fun e1 -> Qpair (e0, e1)) (all_basis_exprs t1))
           (all_basis_exprs t0))

let all_basis_states (t : exprtype) : matrix list =
  List.map expr_to_basis_state (all_basis_exprs t)

let index_to_basis_expr (t : exprtype) (i : int) : expr =
  List.nth (all_basis_exprs t) i

let index_to_basis_state (t : exprtype) (i : int) : matrix =
  List.nth (all_basis_states t) i

let valuation_to_basis_state (tau : valuation) : matrix =
  List.fold_left
    (fun cur (_, e) -> mat_tensor cur (expr_to_basis_state e))
    (mat_identity 1) (StringMap.bindings tau)

let all_context_basis_valuations (d : context) : valuation list =
  let rec iter (bindings : (string * exprtype) list) (cur : valuation list) :
      valuation list =
    match bindings with
    | [] -> cur
    | (x, t) :: l' ->
        iter l'
          (List.flatten
             (List.map
                (fun tau ->
                  List.map (fun e -> StringMap.add x e tau) (all_basis_exprs t))
                cur))
  in
    iter (StringMap.bindings d) [StringMap.empty]

let all_context_basis_states (d : context) : matrix list =
  List.map valuation_to_basis_state (all_context_basis_valuations d)

let index_to_context_basis_valuation (d : context) (i : int) : valuation =
  List.nth (all_context_basis_valuations d) i

let index_to_context_basis_state (d : context) (i : int) : matrix =
  List.nth (all_context_basis_states d) i

(*
Converts an index i of a basis state in the space associated with a context d
to the index of the corresponding basis state in some restriction of the context
*)
let basis_index_restriction (d : context) (fv : StringSet.t) (i : int) : int =
  list_index (StringMap.equal ( = ))
    (all_context_basis_valuations (map_restriction d fv))
    (map_restriction (List.nth (all_context_basis_valuations d) i) fv)

(*
Converts an index i of a basis state in the space associated with a context d
to the index of the corresponding basis state in some context obtained from
merging d with another context g, given a valuation sigma of the context g.
*)
let basis_index_extension (g : context) (sigma : valuation) (d : context)
    (i : int) : int =
  let gd =
    match map_merge false g d with
    | NoneE err -> failwith err
    | SomeE gd -> gd
  in
    list_index (StringMap.equal ( = ))
      (all_context_basis_valuations gd)
      (match
         map_merge false sigma (List.nth (all_context_basis_valuations d) i)
       with
      | NoneE err -> failwith err
      | SomeE tau -> tau)

(*
In a density matrix m corresponding to the context d, traces out all variables
that are not in the specified set fv.
*)
let context_partial_trace (d : context) (fv : StringSet.t) (m : matrix) :
    matrix =
  let d', d0 = map_partition d fv in
    {
      r = context_dimension d';
      c = context_dimension d';
      f =
        (fun i j ->
          complex_sum
            (List.map
               (fun tau ->
                 let i0 = basis_index_extension d0 tau d' i in
                 let j0 = basis_index_extension d0 tau d' j in
                   m.f i0 j0)
               (all_context_basis_valuations d0)));
    }

let rec pure_expr_semantics (g : context) (d : context) (e : expr)
    (sigma : valuation) : matrix =
  let t =
    match pure_type_check g d e with
    | SomeE t -> t
    | NoneE err -> failwith err
  in
  let tdim = type_dimension t in
  let ddim = context_dimension d in
    match e with
    | Null -> expr_to_basis_state Null
    | Var x -> begin
        match StringMap.bindings d with
        | [] -> expr_to_basis_state (StringMap.find x sigma)
        | [(x', _)] when x' = x -> mat_identity tdim
        | _ -> failwith "Error in Var semantics"
      end
    | Qpair (e0, e1) -> begin
        let fv0 = free_vars e0 in
        let fv1 = free_vars e1 in
        let d0 = map_restriction d fv0 in
        let d1 = map_restriction d fv1 in
        let e0sem = pure_expr_semantics g d0 e0 sigma in
        let e1sem = pure_expr_semantics g d1 e1 sigma in
          mat_from_basis_action ddim (fun i ->
              let i0 = basis_index_restriction d fv0 i in
              let i1 = basis_index_restriction d fv1 i in
                mat_tensor (mat_column e0sem i0) (mat_column e1sem i1))
      end
    | Ctrl (e', t0, l, _) -> begin
        let fve' = free_vars e' in
        let g0 = map_restriction g fve' in
        let d0 = map_restriction d fve' in
          match map_merge false g0 d0 with
          | NoneE err -> failwith err
          | SomeE g0d0 ->
              let super0 = mixed_expr_semantics g0d0 e' in
                mat_sum tdim ddim
                  (List.map
                     (fun v ->
                       mat_sum tdim ddim
                         (List.map
                            (fun (ej, ej') ->
                              let gj =
                                match context_check StringMap.empty t0 ej with
                                | NoneE err -> failwith err
                                | SomeE gj -> gj
                              in
                              let ggj =
                                match map_merge false g gj with
                                | NoneE err -> failwith err
                                | SomeE ggj -> ggj
                              in
                                mat_sum tdim ddim
                                  (List.map
                                     (fun sigmaj ->
                                       let sigmaj' =
                                         match
                                           map_merge false sigma sigmaj
                                         with
                                         | NoneE err -> failwith err
                                         | SomeE sigmaj' -> sigmaj'
                                       in
                                       let proj =
                                         mat_to_scalar
                                           (mat_adjoint
                                              (valuation_to_basis_state sigmaj)
                                           *@ mat_adjoint
                                                (pure_expr_semantics
                                                   StringMap.empty gj ej
                                                   StringMap.empty)
                                           *@ v)
                                       in
                                       let ej'_sem =
                                         pure_expr_semantics ggj d ej' sigmaj'
                                       in
                                         mat_from_basis_action ddim (fun i ->
                                             let i0 =
                                               basis_index_restriction d fve' i
                                             in
                                             let i0g0d0 =
                                               basis_index_extension g0 sigma
                                                 d0 i0
                                             in
                                             let prob =
                                               mat_to_scalar
                                                 (mat_adjoint v
                                                 *@ superop_apply super0
                                                      (mat_outer
                                                         (index_to_context_basis_state
                                                            g0d0 i0g0d0))
                                                 *@ v)
                                             in
                                               mat_scalar_mul
                                                 (Complex.mul prob proj)
                                                 ej'_sem
                                               *@ index_to_context_basis_state
                                                    d i))
                                     (all_context_basis_valuations gj)))
                            l))
                     (all_basis_states t0))
      end
    | Try _ -> failwith "Try is not a pure expression"
    | Apply (f, e') ->
        pure_prog_semantics f *@ pure_expr_semantics g d e' sigma

and mixed_expr_semantics (d : context) (e : expr) : superoperator =
  let _ =
    match mixed_type_check d e with
    | SomeE _ -> ()
    | NoneE err -> failwith err
  in
  let ddim = context_dimension d in
    match (e, pure_type_check StringMap.empty d e) with
    | _, SomeE _ ->
        let pure_sem =
          pure_expr_semantics StringMap.empty d e StringMap.empty
        in
          superop_from_basis_action ddim (fun i j ->
              let tau = index_to_context_basis_state d i in
              let tau' = index_to_context_basis_state d j in
                pure_sem *@ tau *@ mat_adjoint tau' *@ mat_adjoint pure_sem)
    | Qpair (e0, e1), _ -> begin
        let fv0 = free_vars e0 in
        let fv1 = free_vars e1 in
        let d0 = map_restriction d fv0 in
        let d1 = map_restriction d fv1 in
        let e0sem = mixed_expr_semantics d0 e0 in
        let e1sem = mixed_expr_semantics d1 e1 in
          superop_from_basis_action ddim (fun i j ->
              let i0 = basis_index_restriction d fv0 i in
              let i1 = basis_index_restriction d fv1 i in
              let j0 = basis_index_restriction d fv0 j in
              let j1 = basis_index_restriction d fv1 j in
                mat_tensor
                  (superop_on_basis e0sem i0 j0)
                  (superop_on_basis e1sem i1 j1))
      end
    | Try (e0, e1), _ -> begin
        let fv0 = free_vars e0 in
        let fv1 = free_vars e1 in
        let d0 = map_restriction d fv0 in
        let d1 = map_restriction d fv1 in
        let e0sem = mixed_expr_semantics d0 e0 in
        let e1sem = mixed_expr_semantics d1 e1 in
          superop_from_basis_action ddim (fun i j ->
              let i0 = basis_index_restriction d fv0 i in
              let i1 = basis_index_restriction d fv1 i in
              let j0 = basis_index_restriction d fv0 j in
              let j1 = basis_index_restriction d fv1 j in
              let mtry = superop_on_basis e0sem i0 j0 in
              let mcatch = superop_on_basis e1sem i1 j1 in
                mat_plus mtry
                  (mat_scalar_mul
                     (Complex.sub
                        (if i0 = j0 then Complex.one else Complex.zero)
                        (mat_trace mtry))
                     mcatch))
      end
    | Apply (f, e'), _ ->
        let fsem = mixed_prog_semantics f in
        let esem = mixed_expr_semantics d e' in
          superop_from_basis_action ddim (fun i j ->
              superop_apply fsem (superop_on_basis esem i j))
    | _ -> failwith "Error in mixed expression semantics"

and pure_prog_semantics (f : prog) : matrix =
  match (f, prog_type_check f) with
  | _, NoneE err -> failwith err
  | _, SomeE (Channel _) ->
      failwith "Attempted pure semantics for mixed program"
  | U3 (theta, phi, lambda), _ ->
      mat_from_u3 (float_of_real theta) (float_of_real phi)
        (float_of_real lambda)
  | Left (t0, _), _ ->
      mat_from_basis_action (type_dimension t0) (fun i ->
          let e = index_to_basis_expr t0 i in
            expr_to_basis_state (Apply (f, e)))
  | Right (_, t1), _ ->
      mat_from_basis_action (type_dimension t1) (fun i ->
          let e = index_to_basis_expr t1 i in
            expr_to_basis_state (Apply (f, e)))
  | Lambda (e, t, e'), _ -> begin
      match context_check StringMap.empty t e with
      | NoneE err -> failwith err
      | SomeE d ->
          pure_expr_semantics StringMap.empty d e' StringMap.empty
          *@ mat_adjoint
               (pure_expr_semantics StringMap.empty d e StringMap.empty)
    end
  | Rphase (t, er, r0, r1), _ -> begin
      match context_check StringMap.empty t er with
      | NoneE err -> failwith err
      | SomeE d -> begin
          let er_sem =
            pure_expr_semantics StringMap.empty d er StringMap.empty
          in
          let er_proj = er_sem *@ mat_adjoint er_sem in
            mat_plus
              (mat_scalar_mul (Complex.polar 1. (float_of_real r0)) er_proj)
              (mat_scalar_mul
                 (Complex.polar 1. (float_of_real r1))
                 (mat_minus (mat_identity (type_dimension t)) er_proj))
        end
    end

and mixed_prog_semantics (f : prog) : superoperator =
  match (f, prog_type_check f) with
  | _, NoneE err -> failwith err
  | _, SomeE (Coherent (t, _)) ->
      let pure_sem = pure_prog_semantics f in
        superop_from_basis_action (type_dimension t) (fun i j ->
            let v = index_to_basis_state t i in
            let v' = index_to_basis_state t j in
              pure_sem *@ v *@ mat_adjoint v' *@ mat_adjoint pure_sem)
  | Lambda (e, t, e'), SomeE (Channel _) -> begin
      match context_check StringMap.empty t e with
      | NoneE err -> failwith err
      | SomeE d ->
          let fve' = free_vars e' in
          let d' = map_restriction d fve' in
          let e_pure_sem =
            pure_expr_semantics StringMap.empty d e StringMap.empty
          in
          let e'_sem = mixed_expr_semantics d' e' in
            superop_from_basis_action (type_dimension t) (fun i j ->
                let v = index_to_basis_state t i in
                let v' = index_to_basis_state t j in
                  superop_apply e'_sem
                    (context_partial_trace d fve'
                       (mat_adjoint e_pure_sem *@ v *@ mat_adjoint v'
                      *@ e_pure_sem)))
    end
  | _, _ -> failwith "Error in mixed program semantics"

let top_pure_expr_semantics (e : expr) : matrix =
  pure_expr_semantics StringMap.empty StringMap.empty e StringMap.empty

let top_mixed_expr_semantics (e : expr) : matrix =
  superop_apply
    (mixed_expr_semantics StringMap.empty e)
    (expr_to_basis_state Null)

let measurement_outcomes (e : expr) : (expr * float) list =
  let t =
    match mixed_type_check StringMap.empty e with
    | NoneE err -> failwith err
    | SomeE t -> t
  in
  let sem = top_mixed_expr_semantics e in
  let basis = all_basis_exprs t in
  let probs =
    List.map
      (fun e' ->
        let v = expr_to_basis_state e' in
          Complex.norm (mat_to_scalar (mat_adjoint v *@ sem *@ v)))
      basis
  in
    List.sort
      (fun (_, p0) (_, p1) ->
        if float_approx_equal p0 p1 then 0 else Stdlib.compare p1 p0)
      ((List.filter (fun (_, p) -> not (float_approx_equal p 0.)))
         (List.combine basis probs))
