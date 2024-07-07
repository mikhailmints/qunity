open Util
open Reals
open Matrix
open Syntax
open Typechecking

type valuation = expr StringMap.t

let memo_size = 100

let string_of_context (d : context) =
  string_of_list
    (fun (x, e) -> Printf.sprintf "%s = %s" x (string_of_type e))
    (StringMap.bindings d)

let string_of_valuation (sigma : valuation) =
  string_of_list
    (fun (x, e) -> Printf.sprintf "%s = %s" x (string_of_expr e))
    (StringMap.bindings sigma)

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

let all_basis_exprs_memo : (exprtype, expr array) Hashtbl.t =
  Hashtbl.create memo_size

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

let all_basis_states_memo : (exprtype, matrix array) Hashtbl.t =
  Hashtbl.create memo_size

let all_basis_states (t : exprtype) : matrix array =
  if Hashtbl.mem all_basis_states_memo t then
    Hashtbl.find all_basis_states_memo t
  else
    let res =
      Array.of_list
        (List.map expr_to_basis_state (Array.to_list (all_basis_exprs t)))
    in
      Hashtbl.add all_basis_states_memo t res;
      res

let index_to_basis_expr (t : exprtype) (i : int) : expr =
  (all_basis_exprs t).(i)

let index_to_basis_state (t : exprtype) (i : int) : matrix =
  (all_basis_states t).(i)

let valuation_to_basis_state (tau : valuation) : matrix =
  List.fold_left
    (fun cur (_, e) -> mat_tensor cur (expr_to_basis_state e))
    (mat_identity 1) (StringMap.bindings tau)

let all_context_basis_valuations_memo : (context, valuation array) Hashtbl.t =
  Hashtbl.create memo_size

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

let all_context_basis_states_memo : (context, matrix array) Hashtbl.t =
  Hashtbl.create memo_size

let all_context_basis_states (d : context) : matrix array =
  if Hashtbl.mem all_context_basis_states_memo d then
    Hashtbl.find all_context_basis_states_memo d
  else
    let res =
      Array.map valuation_to_basis_state (all_context_basis_valuations d)
    in
      Hashtbl.add all_context_basis_states_memo d res;
      res

let index_to_context_basis_valuation (d : context) (i : int) : valuation =
  (all_context_basis_valuations d).(i)

let index_to_context_basis_state (d : context) (i : int) : matrix =
  (all_context_basis_states d).(i)

let basis_index_restriction_memo : (context * StringSet.t * int, int) Hashtbl.t
    =
  Hashtbl.create memo_size

(*
Converts an index i of a basis state in the space associated with a context d
to the index of the corresponding basis state in some restriction of the context
*)
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

let basis_index_extension_memo :
    (context * valuation * context * int, int) Hashtbl.t =
  Hashtbl.create memo_size

(*
Converts an index i of a basis state in the space associated with a context d
to the index of the corresponding basis state in some context obtained from
merging d with another context g, given a valuation sigma of the context g.
*)
let basis_index_extension (g : context) (sigma : valuation) (d : context)
    (i : int) : int =
  if Hashtbl.mem basis_index_extension_memo (g, sigma, d, i) then
    Hashtbl.find basis_index_extension_memo (g, sigma, d, i)
  else
    let res =
      begin
        let gd =
          match map_merge false g d with
          | NoneE err -> failwith err
          | SomeE gd -> gd
        in
        let tau =
          match map_merge false sigma (all_context_basis_valuations d).(i) with
          | NoneE err -> failwith err
          | SomeE tau -> tau
        in
          list_index (StringMap.equal ( = ))
            (Array.to_list (all_context_basis_valuations gd))
            tau
      end
    in
      Hashtbl.add basis_index_extension_memo (g, sigma, d, i) res;
      res

(*
In a density matrix m corresponding to the context d, traces out all variables
that are not in the specified set fv.
*)
let context_partial_trace (d : context) (fv : StringSet.t) (m : matrix) :
    matrix =
  let d', d0 = map_partition d fv in
    mat_from_fun (context_dimension d') (context_dimension d')
      begin
        fun i j ->
          complex_sum
            (List.map
               (fun tau ->
                 let i0 = basis_index_extension d0 tau d' i in
                 let j0 = basis_index_extension d0 tau d' j in
                   mat_entry m i0 j0)
               (Array.to_list (all_context_basis_valuations d0)))
      end

let pure_expr_sem_memo :
    (context * context * expr * valuation, matrix) Hashtbl.t =
  Hashtbl.create memo_size

let mixed_expr_sem_memo : (context * expr, superoperator) Hashtbl.t =
  Hashtbl.create memo_size

let pure_prog_sem_memo : (prog, matrix) Hashtbl.t = Hashtbl.create memo_size

let mixed_prog_sem_memo : (prog, superoperator) Hashtbl.t =
  Hashtbl.create memo_size

let rec pure_expr_semantics (g : context) (d : context) (e : expr)
    (sigma : valuation) : matrix =
  if Hashtbl.mem pure_expr_sem_memo (g, d, e, sigma) then
    Hashtbl.find pure_expr_sem_memo (g, d, e, sigma)
  else
    let res =
      begin
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
                mat_from_basis_action tdim ddim (fun i ->
                    let i0 = basis_index_restriction d fv0 i in
                    let i1 = basis_index_restriction d fv1 i in
                      mat_tensor (mat_column e0sem i0) (mat_column e1sem i1))
            end
          | Ctrl (e', t0, l, t1) -> ctrl_semantics g d sigma e' t0 l t1
          | Try _ -> failwith "Try is not a pure expression"
          | Apply (f, e') ->
              pure_prog_semantics f *@ pure_expr_semantics g d e' sigma
      end
    in
      Hashtbl.add pure_expr_sem_memo (g, d, e, sigma) res;
      res

and ctrl_semantics (g : context) (d : context) (sigma : valuation) (e : expr)
    (t0 : exprtype) (l : (expr * expr) list) (t1 : exprtype) : matrix =
  let tdim = type_dimension t1 in
  let ddim = context_dimension d in
  let fve = free_vars e in
  let g0 = map_restriction g fve in
  let d0 = map_restriction d fve in
    match map_merge false g0 d0 with
    | NoneE err -> failwith err
    | SomeE g0d0 ->
        let super0 = mixed_expr_semantics g0d0 e in
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
                                   match map_merge false sigma sigmaj with
                                   | NoneE err -> failwith err
                                   | SomeE sigmaj' -> sigmaj'
                                 in
                                 let proj =
                                   mat_to_scalar
                                     (mat_adjoint
                                        (valuation_to_basis_state sigmaj)
                                     *@ mat_adjoint
                                          (pure_expr_semantics StringMap.empty
                                             gj ej StringMap.empty)
                                     *@ v)
                                 in
                                   if proj = Complex.zero then
                                     mat_zero tdim ddim
                                   else
                                     let ej'_sem =
                                       pure_expr_semantics ggj d ej' sigmaj'
                                     in
                                       mat_from_basis_action tdim ddim
                                         (fun i ->
                                           let i0 =
                                             basis_index_restriction d fve i
                                           in
                                           let i0g0d0 =
                                             basis_index_extension g0
                                               (map_restriction sigma fve)
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
                                             if prob = Complex.zero then
                                               mat_zero tdim 1
                                             else
                                               mat_scalar_mul
                                                 (Complex.mul prob proj)
                                                 ej'_sem
                                               *@ index_to_context_basis_state
                                                    d i))
                               (Array.to_list
                                  (all_context_basis_valuations gj))))
                      l))
               (Array.to_list (all_basis_states t0)))

and mixed_expr_semantics (d : context) (e : expr) : superoperator =
  if Hashtbl.mem mixed_expr_sem_memo (d, e) then
    Hashtbl.find mixed_expr_sem_memo (d, e)
  else
    let res =
      begin
        let t =
          match mixed_type_check d e with
          | SomeE t -> t
          | NoneE err -> failwith err
        in
        let ddim = context_dimension d in
        let tdim = type_dimension t in
          match (e, pure_type_check StringMap.empty d e) with
          | _, SomeE _ ->
              let pure_sem =
                pure_expr_semantics StringMap.empty d e StringMap.empty
              in
                superop_from_basis_action tdim ddim (fun i j ->
                    let tau = index_to_context_basis_state d i in
                    let tau' = index_to_context_basis_state d j in
                      pure_sem *@ tau *@ mat_adjoint tau'
                      *@ mat_adjoint pure_sem)
          | Qpair (e0, e1), _ -> begin
              let fv0 = free_vars e0 in
              let fv1 = free_vars e1 in
              let d0 = map_restriction d fv0 in
              let d1 = map_restriction d fv1 in
              let e0sem = mixed_expr_semantics d0 e0 in
              let e1sem = mixed_expr_semantics d1 e1 in
                superop_from_basis_action tdim ddim (fun i j ->
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
                superop_from_basis_action tdim ddim (fun i j ->
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
                superop_from_basis_action tdim ddim (fun i j ->
                    superop_apply fsem (superop_on_basis esem i j))
          | _ -> failwith "Error in mixed expression semantics"
      end
    in
      Hashtbl.add mixed_expr_sem_memo (d, e) res;
      res

and pure_prog_semantics (f : prog) : matrix =
  if Hashtbl.mem pure_prog_sem_memo f then
    Hashtbl.find pure_prog_sem_memo f
  else
    let res =
      begin
        match (f, prog_type_check f) with
        | _, NoneE err -> failwith err
        | _, SomeE (Channel _) ->
            failwith "Attempted pure semantics for mixed program"
        | U3 (theta, phi, lambda), _ ->
            mat_from_u3 (float_of_real theta) (float_of_real phi)
              (float_of_real lambda)
        | Left (t0, t1), _ ->
            mat_from_basis_action
              (type_dimension (SumType (t0, t1)))
              (type_dimension t0)
              (fun i ->
                let e = index_to_basis_expr t0 i in
                  expr_to_basis_state (Apply (f, e)))
        | Right (t0, t1), _ ->
            mat_from_basis_action
              (type_dimension (SumType (t0, t1)))
              (type_dimension t1)
              (fun i ->
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
                    (mat_scalar_mul
                       (Complex.polar 1. (float_of_real r0))
                       er_proj)
                    (mat_scalar_mul
                       (Complex.polar 1. (float_of_real r1))
                       (mat_minus (mat_identity (type_dimension t)) er_proj))
              end
          end
      end
    in
      Hashtbl.add pure_prog_sem_memo f res;
      res

and mixed_prog_semantics (f : prog) : superoperator =
  if Hashtbl.mem mixed_prog_sem_memo f then
    Hashtbl.find mixed_prog_sem_memo f
  else
    let res =
      begin
        match (f, prog_type_check f) with
        | _, NoneE err -> failwith err
        | _, SomeE (Coherent (t, t')) ->
            let pure_sem = pure_prog_semantics f in
              superop_from_basis_action (type_dimension t') (type_dimension t)
                (fun i j ->
                  let v = index_to_basis_state t i in
                  let v' = index_to_basis_state t j in
                    pure_sem *@ v *@ mat_adjoint v' *@ mat_adjoint pure_sem)
        | Lambda (e, t, e'), SomeE (Channel (_, t')) -> begin
            match context_check StringMap.empty t e with
            | NoneE err -> failwith err
            | SomeE d ->
                let fve' = free_vars e' in
                let d' = map_restriction d fve' in
                let e_pure_sem =
                  pure_expr_semantics StringMap.empty d e StringMap.empty
                in
                let e'_sem = mixed_expr_semantics d' e' in
                  superop_from_basis_action (type_dimension t')
                    (type_dimension t) (fun i j ->
                      let v = index_to_basis_state t i in
                      let v' = index_to_basis_state t j in
                        superop_apply e'_sem
                          (context_partial_trace d fve'
                             (mat_adjoint e_pure_sem *@ v *@ mat_adjoint v'
                            *@ e_pure_sem)))
          end
        | _, _ -> failwith "Error in mixed program semantics"
      end
    in
      Hashtbl.add mixed_prog_sem_memo f res;
      res

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
      (Array.to_list basis)
  in
    List.sort
      (fun (_, p0) (_, p1) ->
        if float_approx_equal p0 p1 then 0 else Stdlib.compare p1 p0)
      ((List.filter (fun (_, p) -> not (float_approx_equal p 0.)))
         (List.combine (Array.to_list basis) probs))
