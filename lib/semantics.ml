open Util
open Reals
open Matrix
open Syntax
open Typechecking

(** The initial size of hash tables used for memoization. *)
let memo_size = 1000

(** Converts a classical basis expression into a basis vector. *)
let rec expr_to_basis_state (t : exprtype) (e : expr) : matrix =
  match (e, t) with
  | Null, Qunit -> mat_identity 1
  | Apply (Left (t0, t1), e0), SumType (t0', t1') when t0 = t0' && t1 = t1' ->
      vec_dirsum (expr_to_basis_state t0 e0) (vec_zero (type_dimension t1))
  | Apply (Right (t0, t1), e1), SumType (t0', t1') when t0 = t0' && t1 = t1' ->
      vec_dirsum (vec_zero (type_dimension t0)) (expr_to_basis_state t1 e1)
  | Qpair (e0, e1), ProdType (t0, t1) ->
      mat_tensor (expr_to_basis_state t0 e0) (expr_to_basis_state t1 e1)
  | _, _ -> failwith "Type mismatch"

(** Memoization cache for [all_basis_exprs]. *)
let all_basis_exprs_memo : (exprtype, expr array) Hashtbl.t =
  Hashtbl.create memo_size

(** Returns all basis expressions of type [t]. *)
let all_basis_exprs (t : exprtype) : expr array =
  let rec all_basis_exprs_calculate (t : exprtype) : expr list =
    match t with
    | Void -> []
    | Qunit -> [Null]
    | SumType (t0, t1) ->
        List.map
          (fun e0 -> Apply (Left (t0, t1), e0))
          (all_basis_exprs_calculate t0)
        @ List.map
            (fun e1 -> Apply (Right (t0, t1), e1))
            (all_basis_exprs_calculate t1)
    | ProdType (t0, t1) ->
        List.flatten
          (List.map
             (fun e0 ->
               List.map
                 (fun e1 -> Qpair (e0, e1))
                 (all_basis_exprs_calculate t1))
             (all_basis_exprs_calculate t0))
  in
    if Hashtbl.mem all_basis_exprs_memo t then
      Hashtbl.find all_basis_exprs_memo t
    else
      let res = Array.of_list (all_basis_exprs_calculate t) in
        Hashtbl.add all_basis_exprs_memo t res;
        res

(** Memoization cache for [all_basis_states]. *)
let all_basis_states_memo : (exprtype, matrix array) Hashtbl.t =
  Hashtbl.create memo_size

(** Returns all basis states in the vector space corresponding to the type [t]. *)
let all_basis_states (t : exprtype) : matrix array =
  if Hashtbl.mem all_basis_states_memo t then
    Hashtbl.find all_basis_states_memo t
  else
    let res =
      Array.of_list
        (List.map (expr_to_basis_state t) (Array.to_list (all_basis_exprs t)))
    in
      Hashtbl.add all_basis_states_memo t res;
      res

(** Returns the [i]'th basis expression of type [t]. *)
let index_to_basis_expr (t : exprtype) (i : int) : expr =
  (all_basis_exprs t).(i)

(** Returns the [i]'th basis state of the vector space corresponding to type
    [t]. *)
let index_to_basis_state (t : exprtype) (i : int) : matrix =
  (all_basis_states t).(i)

(** Converts a classical valuation into a basis vector. *)
let valuation_to_basis_state (d : context) (tau : valuation) : matrix =
  List.fold_left
    (fun cur (x, e) ->
      let t = StringMap.find x d in
        mat_tensor cur (expr_to_basis_state t e))
    (mat_identity 1) (StringMap.bindings tau)

(** Memoization cache for [all_context_basis_valuations]. *)
let all_context_basis_valuations_memo : (context, valuation array) Hashtbl.t =
  Hashtbl.create memo_size

(** Returns all basis valuations of the context [d]. *)
let all_context_basis_valuations (d : context) : valuation array =
  let rec iter (bindings : (string * exprtype) list) (cur : valuation list) :
      valuation list =
    match bindings with
    | [] -> cur
    | (x, t) :: l' ->
        iter l'
          (List.flatten
             (List.map
                (fun tau ->
                  List.map
                    (fun e -> StringMap.add x e tau)
                    (Array.to_list (all_basis_exprs t)))
                cur))
  in
    if Hashtbl.mem all_context_basis_valuations_memo d then
      Hashtbl.find all_context_basis_valuations_memo d
    else
      let res =
        Array.of_list (iter (StringMap.bindings d) [StringMap.empty])
      in
        Hashtbl.add all_context_basis_valuations_memo d res;
        res

(** Memoization cache for [all_context_basis_states]. *)
let all_context_basis_states_memo : (context, matrix array) Hashtbl.t =
  Hashtbl.create memo_size

(** Returns all basis states in the vector space corresponding to the context
    [d]. *)
let all_context_basis_states (d : context) : matrix array =
  if Hashtbl.mem all_context_basis_states_memo d then
    Hashtbl.find all_context_basis_states_memo d
  else
    let res =
      Array.map (valuation_to_basis_state d) (all_context_basis_valuations d)
    in
      Hashtbl.add all_context_basis_states_memo d res;
      res

(** Returns the [i]'th basis valuation of the context [d]. *)
let index_to_context_basis_valuation (d : context) (i : int) : valuation =
  (all_context_basis_valuations d).(i)

(** Returns the [i]'th basis state of the vector space corresponding to the
    context [d]. *)
let index_to_context_basis_state (d : context) (i : int) : matrix =
  (all_context_basis_states d).(i)

(** Memoization cache for [basis_index_restriction]. *)
let basis_index_restriction_memo : (context * StringSet.t * int, int) Hashtbl.t
    =
  Hashtbl.create memo_size

(** Converts an index [i] of a basis state in the space associated with a
    context [d] to the index of the corresponding basis state in the
    restriction of the context to the variables in the set [fv]. *)
let basis_index_restriction (d : context) (fv : StringSet.t) (i : int) : int =
  if Hashtbl.mem basis_index_restriction_memo (d, fv, i) then
    Hashtbl.find basis_index_restriction_memo (d, fv, i)
  else
    let res =
      list_index (StringMap.equal ( = ))
        (Array.to_list (all_context_basis_valuations (map_restriction d fv)))
        (map_restriction (all_context_basis_valuations d).(i) fv)
    in
      Hashtbl.add basis_index_restriction_memo (d, fv, i) res;
      res

(** Memoization cache for [basis_index_extension]. *)
let basis_index_extension_memo :
    (context * valuation * context * int, int) Hashtbl.t =
  Hashtbl.create memo_size

(** Converts an index [i] of a basis state in the space associated with a
    context [d] to the index of the corresponding basis state in some context
    obtained from merging [d] with another context [g], given a valuation
    [sigma] of the context [g]. *)
let basis_index_extension (g : context) (sigma : valuation) (d : context)
    (i : int) : int =
  if Hashtbl.mem basis_index_extension_memo (g, sigma, d, i) then
    Hashtbl.find basis_index_extension_memo (g, sigma, d, i)
  else
    let res =
      begin
        let gd = map_merge_noopt false g d in
        let tau =
          map_merge_noopt false sigma (all_context_basis_valuations d).(i)
        in
          list_index (StringMap.equal ( = ))
            (Array.to_list (all_context_basis_valuations gd))
            tau
      end
    in
      Hashtbl.add basis_index_extension_memo (g, sigma, d, i) res;
      res

(** In a density matrix [m] corresponding to the context [d], traces out all
    variables that are not in the specified set [fv]. *)
let context_partial_trace (d : context) (fv : StringSet.t) (m : matrix) :
    matrix =
  let d', d0 = map_partition d fv in
  let fv0 = map_dom d0 in
  let d'_dim = context_dimension d' in
    match m.ent with
    | Dense f ->
        let d0_val = Array.to_list (all_context_basis_valuations d0) in
          mat_from_fun d'_dim d'_dim
            begin
              fun i j ->
                complex_sum
                  (List.map
                     (fun tau ->
                       let i0 = basis_index_extension d0 tau d' i in
                       let j0 = basis_index_extension d0 tau d' j in
                         f i0 j0)
                     d0_val)
            end
    | Sparse s ->
        mat_sum d'_dim d'_dim
          ((List.map (fun ((i, j), z) ->
                let i' = basis_index_restriction d fv i in
                let j' = basis_index_restriction d fv j in
                let i0 = basis_index_restriction d fv0 i in
                let j0 = basis_index_restriction d fv0 j in
                  if i0 = j0 then
                    mat_from_map d'_dim d'_dim (Int2Map.singleton (i', j') z)
                  else
                    mat_zero d'_dim d'_dim))
             (Int2Map.bindings s))

(** Memoization cache for [pure_expr_semantics]. *)
let pure_expr_sem_memo : (pure_expr_typing_proof * valuation, matrix) Hashtbl.t
    =
  Hashtbl.create memo_size

(** Memoization cache for [mixed_expr_semantics]. *)
let mixed_expr_sem_memo :
    (mixed_expr_typing_proof * valuation, superoperator) Hashtbl.t =
  Hashtbl.create memo_size

(** Memoization cache for [pure_prog_semantics]. *)
let pure_prog_sem_memo : (pure_prog_typing_proof, matrix) Hashtbl.t =
  Hashtbl.create memo_size

(** Memoization cache for [mixed_prog_semantics]. *)
let mixed_prog_sem_memo : (mixed_prog_typing_proof, superoperator) Hashtbl.t =
  Hashtbl.create memo_size

(** Computes the pure semantics of an expression as a matrix. *)
let rec pure_expr_semantics (tp : pure_expr_typing_proof) (sigma : valuation) :
    matrix =
  if Hashtbl.mem pure_expr_sem_memo (tp, sigma) then
    Hashtbl.find pure_expr_sem_memo (tp, sigma)
  else
    let res =
      begin
        let _, d_whole = context_of_pure_expr_proof tp in
        let t_whole = type_of_pure_expr_proof tp in
        let tdim = type_dimension t_whole in
        let ddim = context_dimension d_whole in
          match tp with
          | TUnit _ -> expr_to_basis_state t_whole Null
          | TCvar { x; _ } ->
              expr_to_basis_state t_whole (StringMap.find x sigma)
          | TQvar _ -> mat_identity tdim
          | TPurePair { d; d0; d1; e0; e1; _ } -> begin
              let dd0 = map_merge_noopt false d d0 in
              let dd1 = map_merge_noopt false d d1 in
              let e0sem = pure_expr_semantics e0 sigma in
              let e1sem = pure_expr_semantics e1 sigma in
                mat_from_basis_action tdim ddim (fun i ->
                    let i0 = basis_index_restriction d_whole (map_dom dd0) i in
                    let i1 = basis_index_restriction d_whole (map_dom dd1) i in
                      mat_tensor (mat_column e0sem i0) (mat_column e1sem i1))
            end
          | TCtrl _ -> ctrl_semantics tp sigma
          | TPureApp { f; e; _ } ->
              pure_prog_semantics f *@ pure_expr_semantics e sigma
      end
    in
      Hashtbl.add pure_expr_sem_memo (tp, sigma) res;
      res

(** Computes the pure semantics of a control expression. *)
and ctrl_semantics (tp : pure_expr_typing_proof) (sigma : valuation) : matrix =
  match tp with
  | TCtrl { t0; t1; d; d'; e; l; _ } -> begin
      let dd' = map_merge_noopt false d d' in
      let tdim = type_dimension t1 in
      let ddim = context_dimension dd' in
      let super0 = mixed_expr_semantics e sigma in
        mat_sum tdim ddim
          (List.map
             (fun v ->
               mat_sum tdim ddim
                 (List.map
                    (fun (gj, ej, ej') ->
                      mat_sum tdim ddim
                        (List.map
                           (fun sigmaj ->
                             let sigmaj' =
                               map_merge_noopt false sigma sigmaj
                             in
                             let proj =
                               mat_to_scalar
                                 (mat_adjoint
                                    (valuation_to_basis_state gj sigmaj)
                                 *@ mat_adjoint
                                      (pure_expr_semantics ej StringMap.empty)
                                 *@ v)
                             in
                               if proj = Complex.zero then
                                 mat_zero tdim ddim
                               else
                                 let ej'_sem =
                                   pure_expr_semantics ej' sigmaj'
                                 in
                                   mat_from_basis_action tdim ddim (fun i ->
                                       let i0 =
                                         basis_index_restriction dd'
                                           (map_dom d) i
                                       in
                                       let prob =
                                         mat_to_scalar
                                           (mat_adjoint v
                                           *@ superop_apply super0
                                                (mat_outer
                                                   (index_to_context_basis_state
                                                      d i0))
                                           *@ v)
                                       in
                                         if prob = Complex.zero then
                                           mat_zero tdim 1
                                         else
                                           mat_scalar_mul
                                             (Complex.mul prob proj) ej'_sem
                                           *@ index_to_context_basis_state dd'
                                                i))
                           (Array.to_list (all_context_basis_valuations gj))))
                    l))
             (Array.to_list (all_basis_states t0)))
    end
  | _ -> failwith "Expected TCtrl"

(** Computes the pure semantics of a match expression. *)
and match_semantics (tp : mixed_expr_typing_proof) (sigma : valuation) :
    superoperator =
  match tp with
  | TMatch { t0; t1; d; d0; d1; e; l; _ } -> begin
      let dd0 = map_merge_noopt false d d0 in
      let dd1 = map_merge_noopt false d d1 in
      let dd0d1 = map_merge_noopt false dd0 d1 in
      let tdim = type_dimension t1 in
      let ddim = context_dimension dd0d1 in
      let super0 = mixed_expr_semantics e sigma in
        superop_from_basis_action tdim ddim (fun i j ->
            let i0 = basis_index_restriction dd0d1 (map_dom dd0) i in
            let j0 = basis_index_restriction dd0d1 (map_dom dd0) j in
            let i1 = basis_index_restriction dd0d1 (map_dom dd1) i in
            let j1 = basis_index_restriction dd0d1 (map_dom dd1) j in
              mat_sum tdim tdim
                (List.map
                   (fun v ->
                     mat_sum tdim tdim
                       (List.map
                          (fun (gj, ej, ej') ->
                            mat_sum tdim tdim
                              (List.map
                                 (fun sigmaj ->
                                   let proj =
                                     mat_to_scalar
                                       (mat_adjoint
                                          (valuation_to_basis_state gj sigmaj)
                                       *@ mat_adjoint
                                            (pure_expr_semantics ej
                                               StringMap.empty)
                                       *@ v)
                                   in
                                     if proj = Complex.zero then
                                       mat_zero tdim tdim
                                     else
                                       let ej'_sem =
                                         mixed_expr_semantics ej'
                                           (map_merge_noopt false sigma sigmaj)
                                       in
                                       let prob =
                                         mat_to_scalar
                                           (mat_adjoint v
                                           *@ superop_apply super0
                                                (index_to_context_basis_state
                                                   dd0 i0
                                                *@ mat_adjoint
                                                     (index_to_context_basis_state
                                                        dd0 j0))
                                           *@ v)
                                       in
                                         if prob = Complex.zero then
                                           mat_zero tdim tdim
                                         else
                                           mat_scalar_mul
                                             (Complex.mul prob proj)
                                             (superop_apply ej'_sem
                                                (index_to_context_basis_state
                                                   dd1 i1
                                                *@ mat_adjoint
                                                     (index_to_context_basis_state
                                                        dd1 j1))))
                                 (Array.to_list
                                    (all_context_basis_valuations gj))))
                          l))
                   (Array.to_list (all_basis_states t0))))
    end
  | _ -> failwith "Expected TMatch"

(** Computes the mixed semantics of an expression as a superoperator. *)
and mixed_expr_semantics (tp : mixed_expr_typing_proof) (sigma : valuation) :
    superoperator =
  if Hashtbl.mem mixed_expr_sem_memo (tp, sigma) then
    Hashtbl.find mixed_expr_sem_memo (tp, sigma)
  else
    let res =
      begin
        let _, d_whole = context_of_mixed_expr_proof tp in
        let t_whole = type_of_mixed_expr_proof tp in
        let tdim = type_dimension t_whole in
        let ddim = context_dimension d_whole in
          match tp with
          | TMix tp' ->
              let pure_sem = pure_expr_semantics tp' sigma in
                superop_from_basis_action tdim ddim (fun i j ->
                    let tau = index_to_context_basis_state d_whole i in
                    let tau' = index_to_context_basis_state d_whole j in
                      pure_sem *@ tau *@ mat_adjoint tau'
                      *@ mat_adjoint pure_sem)
          | TDiscard { d; d0; e; _ } -> begin
              let dd0 = map_merge_noopt false d d0 in
              let fve = map_dom d in
              let e_sem = mixed_expr_semantics e sigma in
                superop_from_basis_action tdim ddim (fun i j ->
                    let v = index_to_context_basis_state dd0 i in
                    let v' = index_to_context_basis_state dd0 j in
                      superop_apply e_sem
                        (context_partial_trace dd0 fve (v *@ mat_adjoint v')))
            end
          | TMixedPair { d; d0; d1; e0; e1; _ } -> begin
              let dd0 = map_merge_noopt false d d0 in
              let dd1 = map_merge_noopt false d d1 in
              let fv0 = map_dom dd0 in
              let fv1 = map_dom dd1 in
              let e0sem = mixed_expr_semantics e0 sigma in
              let e1sem = mixed_expr_semantics e1 sigma in
                superop_from_basis_action tdim ddim (fun i j ->
                    let i0 = basis_index_restriction d_whole fv0 i in
                    let i1 = basis_index_restriction d_whole fv1 i in
                    let j0 = basis_index_restriction d_whole fv0 j in
                    let j1 = basis_index_restriction d_whole fv1 j in
                      mat_tensor
                        (superop_on_basis e0sem i0 j0)
                        (superop_on_basis e1sem i1 j1))
            end
          | TMatch _ -> match_semantics tp sigma
          | TTry { d0; d1; e0; e1; _ } -> begin
              let fv0 = map_dom d0 in
              let fv1 = map_dom d1 in
              let e0sem = mixed_expr_semantics e0 sigma in
              let e1sem = mixed_expr_semantics e1 sigma in
                superop_from_basis_action tdim ddim (fun i j ->
                    let i0 = basis_index_restriction d_whole fv0 i in
                    let i1 = basis_index_restriction d_whole fv1 i in
                    let j0 = basis_index_restriction d_whole fv0 j in
                    let j1 = basis_index_restriction d_whole fv1 j in
                    let mtry = superop_on_basis e0sem i0 j0 in
                    let mcatch = superop_on_basis e1sem i1 j1 in
                      mat_plus mtry
                        (mat_scalar_mul
                           (Complex.sub
                              (if i0 = j0 then Complex.one else Complex.zero)
                              (mat_trace mtry))
                           mcatch))
            end
          | TMixedApp { f; e; _ } ->
              let fsem = mixed_prog_semantics f in
              let esem = mixed_expr_semantics e sigma in
                superop_from_basis_action tdim ddim (fun i j ->
                    superop_apply fsem (superop_on_basis esem i j))
      end
    in
      Hashtbl.add mixed_expr_sem_memo (tp, sigma) res;
      res

(** Computes the pure semantics of a program as a matrix. *)
and pure_prog_semantics (tp : pure_prog_typing_proof) : matrix =
  if Hashtbl.mem pure_prog_sem_memo tp then
    Hashtbl.find pure_prog_sem_memo tp
  else
    let res =
      begin
        match tp with
        | TGate (theta, phi, lambda) ->
            mat_from_u3 (float_of_real theta) (float_of_real phi)
              (float_of_real lambda)
        | TLeft (t0, t1) ->
            mat_from_basis_action
              (type_dimension (SumType (t0, t1)))
              (type_dimension t0)
              (fun i ->
                let e = index_to_basis_expr t0 i in
                  expr_to_basis_state
                    (SumType (t0, t1))
                    (Apply (Left (t0, t1), e)))
        | TRight (t0, t1) ->
            mat_from_basis_action
              (type_dimension (SumType (t0, t1)))
              (type_dimension t1)
              (fun i ->
                let e = index_to_basis_expr t1 i in
                  expr_to_basis_state
                    (SumType (t0, t1))
                    (Apply (Right (t0, t1), e)))
        | TPureAbs { e; e'; _ } -> begin
            pure_expr_semantics e' StringMap.empty
            *@ mat_adjoint (pure_expr_semantics e StringMap.empty)
          end
        | TRphase { t; e; r0; r1; _ } -> begin
            let e_sem = pure_expr_semantics e StringMap.empty in
            let er_proj = e_sem *@ mat_adjoint e_sem in
              mat_plus
                (mat_scalar_mul (Complex.polar 1. (float_of_real r0)) er_proj)
                (mat_scalar_mul
                   (Complex.polar 1. (float_of_real r1))
                   (mat_minus (mat_identity (type_dimension t)) er_proj))
          end
        | TPmatch { t0; t1; l; _ } -> begin
            mat_sum (type_dimension t1) (type_dimension t0)
              (List.map
                 (fun (_, ej, ej') ->
                   pure_expr_semantics ej' StringMap.empty
                   *@ mat_adjoint (pure_expr_semantics ej StringMap.empty))
                 l)
          end
      end
    in
      Hashtbl.add pure_prog_sem_memo tp res;
      res

(** Computes the mixed semantics of a program as a superoperator. *)
and mixed_prog_semantics (tp : mixed_prog_typing_proof) : superoperator =
  if Hashtbl.mem mixed_prog_sem_memo tp then
    Hashtbl.find mixed_prog_sem_memo tp
  else
    let res =
      begin
        match tp with
        | TChannel f' ->
            let t, t' = type_of_prog_proof (PureProg f') in
            let pure_sem = pure_prog_semantics f' in
              superop_from_basis_action (type_dimension t') (type_dimension t)
                (fun i j ->
                  let v = index_to_basis_state t i in
                  let v' = index_to_basis_state t j in
                    pure_sem *@ v *@ mat_adjoint v' *@ mat_adjoint pure_sem)
        | TMixedAbs { t; t'; e; e'; _ } -> begin
            let e_pure_sem = pure_expr_semantics e StringMap.empty in
            let e'_sem = mixed_expr_semantics e' StringMap.empty in
              superop_from_basis_action (type_dimension t') (type_dimension t)
                (fun i j ->
                  let v = index_to_basis_state t i in
                  let v' = index_to_basis_state t j in
                    superop_apply e'_sem
                      (mat_adjoint e_pure_sem *@ v *@ mat_adjoint v'
                     *@ e_pure_sem))
          end
      end
    in
      Hashtbl.add mixed_prog_sem_memo tp res;
      res

(** Typechecks an expression and computes its pure semantics. *)
let top_pure_expr_semantics (e : expr) : matrix =
  match pure_type_check StringMap.empty StringMap.empty e with
  | SomeE tp -> pure_expr_semantics tp StringMap.empty
  | NoneE err -> failwith err

(** Typechecks an expression and computes its mixed semantics. *)
let top_mixed_expr_semantics (e : expr) : matrix =
  match mixed_type_check StringMap.empty StringMap.empty e with
  | SomeE tp ->
      superop_apply
        (mixed_expr_semantics tp StringMap.empty)
        (expr_to_basis_state Qunit Null)
  | NoneE err -> failwith err

(** Typechecks a program and computes its pure semantics. *)
let top_pure_prog_semantics (f : prog) : matrix =
  match prog_type_check f with
  | SomeE (PureProg tp) -> pure_prog_semantics tp
  | SomeE (MixedProg _) -> failwith "Expected pure program"
  | NoneE err -> failwith err

(** Typechecks a program and computes its mixed semantics. *)
let top_mixed_prog_semantics (f : prog) : superoperator =
  match prog_type_check f with
  | SomeE (PureProg tp) -> mixed_prog_semantics (TChannel tp)
  | SomeE (MixedProg tp) -> mixed_prog_semantics tp
  | NoneE err -> failwith err

(** Determines the possible measurement outcomes of an expression using the
    Born rule. *)
let measurement_outcomes (e : expr) : (expr * float) list =
  match mixed_type_check StringMap.empty StringMap.empty e with
  | NoneE err -> failwith err
  | SomeE tp -> begin
      let t = type_of_mixed_expr_proof tp in
      let sem = top_mixed_expr_semantics e in
      let basis = all_basis_exprs t in
      let probs =
        List.map
          (fun e' ->
            let v = expr_to_basis_state t e' in
              Complex.norm (mat_to_scalar (mat_adjoint v *@ sem *@ v)))
          (Array.to_list basis)
      in
        List.sort
          (fun (_, p0) (_, p1) ->
            if float_approx_equal p0 p1 then 0 else Stdlib.compare p1 p0)
          ((List.filter (fun (_, p) -> not (float_approx_equal p 0.)))
             (List.combine (Array.to_list basis) probs))
    end
