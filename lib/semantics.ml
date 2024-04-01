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
    | Null, Qunit -> mat_id 1
    | Apply (Left (t0, t1), e0), SumType (t0', t1') when t0 = t0' && t1 = t1'
      ->
        mat_dirsum (expr_to_basis_state e0) (vec_zero (type_dimension t1))
    | Apply (Right (t0, t1), e1), SumType (t0', t1') when t0 = t0' && t1 = t1'
      ->
        mat_dirsum (vec_zero (type_dimension t0)) (expr_to_basis_state e1)
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

let valuation_to_basis_state (tau : valuation) : matrix =
  List.fold_left
    (fun cur (_, e) -> mat_tensor cur (expr_to_basis_state e))
    (mat_id 1) (StringMap.bindings tau)

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

let index_to_basis_valuation (d : context) (i : int) : valuation =
  List.nth (all_context_basis_valuations d) i

let index_to_basis_state (d : context) (i : int) : matrix =
  List.nth (all_context_basis_states d) i

(*
Converts an index i of a basis state in the space associated with a context d
to the index of the corresponding basis state in some restriction of the context
*)
let basis_index_restriction (d : context) (fv : StringSet.t) (i : int) : int =
  list_index
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
    list_index
      (all_context_basis_valuations gd)
      (match
         map_merge false sigma (List.nth (all_context_basis_valuations d) i)
       with
      | NoneE err -> failwith err
      | SomeE tau -> tau)

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
        | [(x', _)] when x' = x -> mat_id tdim
        | _ -> failwith "Error in Var semantics"
      end
    | Qpair (e0, e1) -> begin
        let fv0 = free_vars e0 in
        let fv1 = free_vars e1 in
        let d0 = map_restriction d fv0 in
        let d1 = map_restriction d fv1 in
        let sigma0 = map_restriction sigma fv0 in
        let sigma1 = map_restriction sigma fv1 in
          mat_from_basis_action ddim (fun i ->
              let i0 = basis_index_restriction d fv0 i in
              let i1 = basis_index_restriction d fv1 i in
                mat_tensor
                  (mat_column (pure_expr_semantics g d0 e sigma0) i0)
                  (mat_column (pure_expr_semantics g d1 e sigma1) i1))
      end
    | Ctrl (e', t0, l, _) -> begin
        let fve' = free_vars e' in
        let g0 = map_restriction g fve' in
        let d0 = map_restriction d fve' in
          match map_merge false g0 d0 with
          | NoneE err -> failwith err
          | SomeE g0d0 ->
              let super0 = mixed_expr_semantics g0d0 e' in
                mat_from_basis_action ddim (fun i ->
                    let i0 = basis_index_restriction d fve' i in
                    let i0g0d0 = basis_index_extension g0 sigma d0 i0 in
                      mat_sum tdim ddim
                        (List.map
                           (fun v ->
                             mat_adjoint v
                             *@ superoperator_apply super0
                                  (mat_outer
                                     (index_to_basis_state g0d0 i0g0d0))
                             *@ v
                             *@ mat_sum tdim ddim
                                  (List.map
                                     (fun (ej, ej') ->
                                       let gj =
                                         match
                                           context_check StringMap.empty t0 ej
                                         with
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
                                                    map_merge false sigma
                                                      sigmaj
                                                  with
                                                  | NoneE err -> failwith err
                                                  | SomeE sigmaj' -> sigmaj'
                                                in
                                                  mat_adjoint
                                                    (valuation_to_basis_state
                                                       sigmaj)
                                                  *@ mat_adjoint
                                                       (pure_expr_semantics
                                                          StringMap.empty gj ej
                                                          StringMap.empty)
                                                  *@ v
                                                  *@ pure_expr_semantics ggj d
                                                       ej' sigmaj'
                                                  *@ index_to_basis_state d i)
                                              (all_context_basis_valuations gj)))
                                     l))
                           (all_basis_states t0)))
      end
    | Try _ -> failwith "Try is not a pure expression"
    | Apply (f, e') ->
        pure_prog_semantics f *@ pure_expr_semantics g d e' sigma

and mixed_expr_semantics (d : context) (e : expr) : superoperator =
  failwith "TODO"

and pure_prog_semantics (f : prog) : matrix = failwith "TODO"
and mixed_prog_semantics (f : prog) : superoperator = failwith "TODO"
