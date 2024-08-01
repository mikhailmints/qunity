open Util
open Reals
open Syntax

type pure_expr_typing_proof =
  | TUnit of context (* Γ *)
  | TCvar of exprtype * context * string (* T, ΓΓ', x *)
  | TQvar of exprtype * context * string (* Γ, x, T *)
  | TPurePair of
      exprtype (* T0 *)
      * exprtype (* T1 *)
      * context (* Γ *)
      * context (* Δ *)
      * context (* Δ0 *)
      * context (* Δ1 *)
      * pure_expr_typing_proof (* e0 : T0 *)
      * pure_expr_typing_proof (* e1 : T1 *)
      * bool (* iso *)
  | TCtrl of
      exprtype (* T *)
      * exprtype (* T' *)
      * context (* Γ *)
      * context (* Γ' *)
      * context (* Δ *)
      * context (* Δ' *)
      * mixed_expr_typing_proof (* e : T *)
      * (context * pure_expr_typing_proof * pure_expr_typing_proof) list
      (* Γj, ej : T, ej' : T' *)
      * ortho_proof
      * erasure_proof StringMap.t
      * bool (* iso *)
  | TPureApp of
      exprtype (* T *)
      * exprtype (* T' *)
      * context (* Γ *)
      * context (* Δ *)
      * pure_prog_typing_proof (* f : T -> T' *)
      * pure_expr_typing_proof (* e : T *)
      * bool (* iso *)

and mixed_expr_typing_proof =
  | TMix of pure_expr_typing_proof
  | TMixedPair of
      exprtype (* T0 *)
      * exprtype (* T1 *)
      * context (* Δ *)
      * context (* Δ0 *)
      * context (* Δ1 *)
      * mixed_expr_typing_proof (* e0 : T0 *)
      * mixed_expr_typing_proof (* e1 : T1 *)
      * bool (* iso *)
  | TTry of
      exprtype (* T *)
      * context (* Δ0 *)
      * context (* Δ1 *)
      * mixed_expr_typing_proof (* e0 : T *)
      * mixed_expr_typing_proof (* e1 : T *)
      * bool (* iso *)
  | TMixedApp of
      exprtype (* T *)
      * exprtype (* T' *)
      * context (* Δ *)
      * mixed_prog_typing_proof (* f : T -> T' *)
      * mixed_expr_typing_proof (* e : T *)
      * bool (* iso *)

and pure_prog_typing_proof =
  | TGate of real * real * real
  | TLeft of exprtype * exprtype
  | TRight of exprtype * exprtype
  | TPureAbs of
      exprtype (* T *)
      * exprtype (* T' *)
      * context (* Δ *)
      * pure_expr_typing_proof (* e : T *)
      * pure_expr_typing_proof (* e' : T' *)
      * bool (* iso *)
  | TRphase of
      exprtype (* T *)
      * pure_expr_typing_proof (* e : T *)
      * real (* r *)
      * real (* r' *)

and mixed_prog_typing_proof =
  | TChannel of pure_prog_typing_proof
  | TMixedAbs of
      exprtype (* T *)
      * exprtype (* T' *)
      * context (* Δ *)
      * context (* Δ0 *)
      * pure_expr_typing_proof (* e : T *)
      * mixed_expr_typing_proof (* e' : T' *)
      * bool (* iso *)

and prog_typing_proof =
  | PureProg of pure_prog_typing_proof
  | MixedProg of mixed_prog_typing_proof

and erasure_proof =
  | EVar of exprtype
  | EPair0 of exprtype * exprtype * erasure_proof
  | EPair1 of exprtype * exprtype * erasure_proof

and spanning_proof =
  | SVoid
  | SUnit
  | SVar of exprtype
  | SSum of exprtype * exprtype * spanning_proof * spanning_proof * int * int
  | SPair of
      exprtype
      * exprtype
      * spanning_proof
      * spanning_proof list
      * int
      * int list

and ortho_proof =
  spanning_proof
  * expr list (* spanning list *)
  * expr list (* ortho list, subsequence of spanning list *)

let type_of_pure_expr_proof (tp : pure_expr_typing_proof) : exprtype =
  match tp with
  | TUnit _ -> Qunit
  | TCvar (t, _, _) -> t
  | TQvar (t, _, _) -> t
  | TPurePair (t0, t1, _, _, _, _, _, _, _) -> ProdType (t0, t1)
  | TCtrl (_, t, _, _, _, _, _, _, _, _, _) -> t
  | TPureApp (_, t, _, _, _, _, _) -> t

let type_of_mixed_expr_proof (tp : mixed_expr_typing_proof) : exprtype =
  match tp with
  | TMix p -> type_of_pure_expr_proof p
  | TMixedPair (t0, t1, _, _, _, _, _, _) -> ProdType (t0, t1)
  | TTry (t, _, _, _, _, _) -> t
  | TMixedApp (_, t, _, _, _, _) -> t

let rec type_of_prog_proof (tp : prog_typing_proof) : exprtype * exprtype =
  match tp with
  | PureProg (TGate _) -> (bit, bit)
  | PureProg (TLeft (t0, t1)) -> (t0, SumType (t0, t1))
  | PureProg (TRight (t0, t1)) -> (t1, SumType (t0, t1))
  | PureProg (TPureAbs (t, t', _, _, _, _)) -> (t, t')
  | PureProg (TRphase (t, _, _, _)) -> (t, t)
  | MixedProg (TChannel tp') -> type_of_prog_proof (PureProg tp')
  | MixedProg (TMixedAbs (t, t', _, _, _, _, _)) -> (t, t')

let progtype_of_prog_proof (tp : prog_typing_proof) : progtype =
  let t, t' = type_of_prog_proof tp in
    match tp with
    | PureProg _ -> Coherent (t, t')
    | MixedProg _ -> Channel (t, t')

let context_of_pure_expr_proof (tp : pure_expr_typing_proof) :
    context * context =
  match tp with
  | TUnit g -> (g, StringMap.empty)
  | TCvar (_, g, _) -> (g, StringMap.empty)
  | TQvar (_, d, _) -> (StringMap.empty, d)
  | TPurePair (_, _, g, d, d0, d1, _, _, _) ->
      (g, map_merge_noopt false d (map_merge_noopt false d0 d1))
  | TCtrl (_, _, g, g', d, d', _, _, _, _, _) ->
      (map_merge_noopt false g g', map_merge_noopt false d d')
  | TPureApp (_, _, g, d, _, _, _) -> (g, d)

let context_of_mixed_expr_proof (tp : mixed_expr_typing_proof) : context =
  match tp with
  | TMix tp' -> snd (context_of_pure_expr_proof tp')
  | TMixedPair (_, _, d, d0, d1, _, _, _) ->
      map_merge_noopt false d (map_merge_noopt false d0 d1)
  | TTry (_, d0, d1, _, _, _) -> map_merge_noopt false d0 d1
  | TMixedApp (_, _, d, _, _, _) -> d

let is_iso_pure_expr_proof (tp : pure_expr_typing_proof) : bool =
  match tp with
  | TUnit _
  | TCvar _
  | TQvar _ ->
      true
  | TPurePair (_, _, _, _, _, _, _, _, iso) -> iso
  | TCtrl (_, _, _, _, _, _, _, _, _, _, iso) -> iso
  | TPureApp (_, _, _, _, _, _, iso) -> iso

let is_iso_mixed_expr_proof (tp : mixed_expr_typing_proof) : bool =
  match tp with
  | TMix tp' -> is_iso_pure_expr_proof tp'
  | TMixedPair (_, _, _, _, _, _, _, iso) -> iso
  | TTry (_, _, _, _, _, iso) -> iso
  | TMixedApp (_, _, _, _, _, iso) -> iso

let is_iso_pure_prog_proof (tp : pure_prog_typing_proof) : bool =
  match tp with
  | TGate _
  | TLeft _
  | TRight _
  | TRphase _ ->
      true
  | TPureAbs (_, _, _, _, _, iso) -> iso

let is_iso_mixed_prog_proof (tp : mixed_prog_typing_proof) : bool =
  match tp with
  | TChannel tp' -> is_iso_pure_prog_proof tp'
  | TMixedAbs (_, _, _, _, _, _, iso) -> iso

let is_iso_prog_proof (tp : prog_typing_proof) : bool =
  match tp with
  | PureProg tp' -> is_iso_pure_prog_proof tp'
  | MixedProg tp' -> is_iso_mixed_prog_proof tp'

let rec expr_is_classical (e : expr) : bool =
  match e with
  | Null
  | Var _ ->
      true
  | Qpair (e0, e1) -> expr_is_classical e0 && expr_is_classical e1
  | Ctrl (e', _, l, _) ->
      expr_is_classical e'
      && List.for_all
           (fun (ej, ej') -> expr_is_classical ej && expr_is_classical ej')
           l
  | Try (e0, e1) -> expr_is_classical e0 && expr_is_classical e1
  | Apply (f, e') -> prog_is_classical f && expr_is_classical e'

and prog_is_classical (f : prog) : bool =
  match f with
  | U3 _
  | Rphase _ ->
      false
  | Left _
  | Right _ ->
      true
  | Lambda (e0, _, e1) -> expr_is_classical e0 && expr_is_classical e1

let is_spanning_ortho_proof (orp : ortho_proof) : bool =
  let _, span_list, ortho_list = orp in
    span_list = ortho_list

(*
Finds the free variables in an expression e.
*)
let rec free_vars (e : expr) : StringSet.t =
  match e with
  | Null -> StringSet.empty
  | Var x -> StringSet.singleton x
  | Qpair (e1, e2)
  | Try (e1, e2) ->
      StringSet.union (free_vars e1) (free_vars e2)
  | Ctrl (e', _, l, _) ->
      List.fold_right
        (fun (ej, ej') rest ->
          (* The free variables of the left-hand side patterns in a control
             block are not considered free variables in the expression. *)
          StringSet.union (StringSet.diff (free_vars ej') (free_vars ej)) rest)
        l (free_vars e')
  | Apply (_, e') -> free_vars e'

let rec dephase (e : expr) : expr list =
  match e with
  | Apply (Rphase (_, _, r0, r1), e') when r0 = r1 -> dephase e'
  | Ctrl (_, _, l, _) ->
      List.flatten (List.map (fun (_, ej') -> dephase ej') l)
  | _ -> [e]

(*
Given a list l of Qpairs, split the list into two lists.
*)
let rec split_qpair_list (l : expr list) : (expr list * expr list) option =
  match l with
  | [] -> Some ([], [])
  | Qpair (e0, e1) :: l' -> begin
      match split_qpair_list l' with
      | Some (l0, l1) -> Some (e0 :: l0, e1 :: l1)
      | None -> None
    end
  | _ -> None

let rec erases_check (x : string) (l : expr list) (t : exprtype) :
    erasure_proof option =
  let l' = List.flatten (List.map dephase l) in
    if List.for_all (fun e' -> e' = Var x) l' then
      Some (EVar t)
    else
      match (split_qpair_list l', t) with
      | Some (l0, l1), ProdType (t0, t1) -> begin
          match (erases_check x l0 t0, erases_check x l1 t1) with
          | Some ep, _ -> Some (EPair0 (t0, t1, ep))
          | _, Some ep -> Some (EPair1 (t0, t1, ep))
          | _ -> None
        end
      | _ -> None

(*
Given a list l of expressionsexpected to be of type SumType (t0, t1),
split the list into two lists - one containing all the "left" expressions
of type t0 and one containing all the "right" exressions of type t1.
*)
let rec split_sum_list (t0 : exprtype) (t1 : exprtype) (l : expr list) :
    (expr list * expr list) option =
  match l with
  | [] -> Some ([], [])
  | Apply (Left (t0', t1'), e0) :: l' -> begin
      match (t0 = t0', t1 = t1', split_sum_list t0 t1 l') with
      | true, true, Some (l0, l1) -> Some (e0 :: l0, l1)
      | _, _, _ -> None
    end
  | Apply (Right (t0', t1'), e1) :: l' -> begin
      match (t0 = t0', t1 = t1', split_sum_list t0 t1 l') with
      | true, true, Some (l0, l1) -> Some (l0, e1 :: l1)
      | _, _, _ -> None
    end
  | _ -> None

(*
Adds a pair of expressions (e0, e1) to a list consisting of pairs of expressions
and expression lists. If e0 occurs as one of these expressions, e1 is added
to its corresponding list. Otherwise, the pair consisting of e0 and the
singleton list [e1] is added to the list of pairs
*)
let rec add_to_qpair_list (e0 : expr) (e1 : expr) (l : (expr * expr list) list)
    : (expr * expr list) list =
  match l with
  | [] -> [(e0, [e1])]
  | (e0', l1) :: l' ->
      if e0' = e0 then
        (e0', e1 :: l1) :: l'
      else
        (e0', l1) :: add_to_qpair_list e0 e1 l'

(*
Convert a list of Qpairs of expressions (of type ProdType (t0, t1)) into a
list associating each unique expression of type t0 with a list of all
expressions of type t1 with which it appears in the original list.
*)
let rec spread_qpair_list (l : expr list) : (expr * expr list) list option =
  match l with
  | [] -> Some []
  | Qpair (e0, e1) :: l' -> begin
      match spread_qpair_list l' with
      | Some l'' -> Some (add_to_qpair_list e0 e1 l'')
      | None -> None
    end
  | _ -> None

(*
Comparison for two expressions (assumed to be of the same type).
If the expressions can't both be on the left-hand side of a control block,
the output is meaningless.
*)
and expr_compare (e0 : expr) (e1 : expr) =
  match (e0, e1) with
  | Null, Null -> 0
  | Apply (Left _, _), Apply (Right _, _) -> -1
  | Apply (Right _, _), Apply (Left _, _) -> 1
  | Qpair (e00, e01), Qpair (e10, e11) ->
      let c0 = expr_compare e00 e10 in
        if c0 = 0 then expr_compare e01 e11 else c0
  | _ -> 0

let missing_span (t : exprtype) (l : expr list) :
    (expr list * spanning_proof) option =
  let rec missing_span_helper (t : exprtype) (l : expr list) (fv : StringSet.t)
      : (expr list * spanning_proof) option =
    match (t, l) with
    | Void, [] -> Some ([], SVoid)
    | Qunit, [] -> Some ([Null], SUnit)
    | Qunit, [Null] -> Some ([], SUnit)
    | _, [] -> Some ([Var (fresh_string "$" fv)], SVar t)
    | _, [Var x] -> if StringSet.mem x fv then None else Some ([], SVar t)
    | SumType (t0, t1), _ -> begin
        match split_sum_list t0 t1 l with
        | None -> None
        | Some (l0, l1) -> begin
            match
              (missing_span_helper t0 l0 fv, missing_span_helper t1 l1 fv)
            with
            | Some (l0', sp0), Some (l1', sp1) ->
                Some
                  ( List.map (fun x -> Apply (Left (t0, t1), x)) l0'
                    @ List.map (fun x -> Apply (Right (t0, t1), x)) l1',
                    SSum
                      ( t0,
                        t1,
                        sp0,
                        sp1,
                        List.length (l0 @ l0'),
                        List.length (l1 @ l1') ) )
            | _, _ -> None
          end
      end
    | ProdType (t0, t1), _ -> begin
        match spread_qpair_list l with
        | None -> None
        | Some l' -> begin
            let next_fv =
              List.fold_right StringSet.union
                (List.map free_vars (List.flatten (List.map snd l')))
                fv
            in
              match missing_span_helper t0 (List.map fst l') next_fv with
              | None -> None
              | Some (l0, sp0) -> begin
                  let l'' =
                    List.sort
                      (fun (e0, _) (e1, _) -> expr_compare e0 e1)
                      (List.map (fun e0 -> (e0, [])) l0 @ l')
                  in
                  let result =
                    all_or_nothing
                      (List.map
                         (fun (e0, l1) ->
                           match
                             missing_span_helper t1 l1
                               (StringSet.union fv (free_vars e0))
                           with
                           | Some (l1', tp1) ->
                               Some
                                 (List.map (fun e1 -> Qpair (e0, e1)) l1', tp1)
                           | None -> None)
                         l'')
                  in
                    match result with
                    | Some r ->
                        let njs =
                          List.map
                            (fun (a, b) -> a + b)
                            (List.combine
                               (List.map List.length (List.map fst r))
                               (List.map List.length (List.map snd l'')))
                        in
                          Some
                            ( List.flatten (List.map fst r),
                              SPair
                                ( t0,
                                  t1,
                                  sp0,
                                  List.map snd r,
                                  List.length l'',
                                  njs ) )
                    | None -> None
                end
          end
      end
    | _, _ -> None
  in
    missing_span_helper t l StringSet.empty

(*
Given an expression type t and list l of expressions, expected to be of
type t, extends the list to one "spanning" t.
*)
let span_list (t : exprtype) (l : expr list) : expr list option =
  match missing_span t l with
  | Some (l', _) -> Some (l @ l')
  | None -> None

let ortho_check (t : exprtype) (l : expr list) : ortho_proof option =
  match missing_span t l with
  | Some (l', sp) -> Some (sp, l @ l', l)
  | None -> None

(*
In T-CTRL, given a classical context g and a pair ej, ej', the quantum
context of ej has to be some gj with no classical context - then,
with the classical context g ++ gj, this finds the quantum context of ej'.
*)
let rec first_pattern_context_check (g : context) (t : exprtype)
    (t' : exprtype) (ej : expr) (ej' : expr) : context optionE =
  match context_check StringMap.empty t ej with
  | NoneE err -> NoneE err
  | SomeE gj -> begin
      match map_merge false g gj with
      | SomeE ggj -> context_check ggj t' ej'
      | NoneE err -> NoneE err
    end

(*
In T-CTRL, checks the type of an expression on the right-hand side of a
control block under the contexts obtained from the first pattern in the block.
*)
and pattern_type_check (g : context) (d : context) (t : exprtype)
    (t' : exprtype) ((ej, ej') : expr * expr) :
    (context * pure_expr_typing_proof * pure_expr_typing_proof) option =
  match context_check StringMap.empty t ej with
  | NoneE _ -> None
  | SomeE gj -> begin
      match map_merge false g gj with
      | SomeE ggj -> begin
          match
            (pure_type_check StringMap.empty gj ej, pure_type_check ggj d ej')
          with
          | SomeE tpej, SomeE tpej' when type_of_pure_expr_proof tpej' = t' ->
              Some (gj, tpej, tpej')
          | _ -> None
        end
      | NoneE _ -> None
    end

(*
Finds the type of a mixed expression e with a quantum context d
*)
and mixed_type_check (d : context) (e : expr) : mixed_expr_typing_proof optionE
    =
  match e with
  (* T-MIXEDPAIR *)
  | Qpair (e0, e1) -> begin
      let fv0 = free_vars e0 in
      let fv1 = free_vars e1 in
        if
          StringSet.diff (map_dom d)
            (StringSet.union (free_vars e0) (free_vars e1))
          <> StringSet.empty
        then
          NoneE "Irrelevant variables in quantum context in mixed Qpair"
        else
          let d', d_xor = map_partition d (StringSet.inter fv0 fv1) in
          let d0, d1 = map_partition d_xor fv0 in
            match
              ( mixed_type_check (map_restriction d (free_vars e0)) e0,
                mixed_type_check (map_restriction d (free_vars e1)) e1 )
            with
            | SomeE tp0, SomeE tp1 ->
                SomeE
                  (TMixedPair
                     ( type_of_mixed_expr_proof tp0,
                       type_of_mixed_expr_proof tp1,
                       d',
                       d0,
                       d1,
                       tp0,
                       tp1,
                       is_iso_mixed_expr_proof tp0
                       && is_iso_mixed_expr_proof tp1 ))
            | NoneE err, _
            | _, NoneE err ->
                NoneE (err ^ "\nin mixed Qpair")
    end
  (* T-TRY *)
  | Try (e0, e1) -> begin
      let d0, d1 = map_partition d (free_vars e0) in
        match (mixed_type_check d0 e0, mixed_type_check d1 e1) with
        | SomeE tp0, SomeE tp1 ->
            let t0 = type_of_mixed_expr_proof tp0 in
            let t1 = type_of_mixed_expr_proof tp1 in
              if t0 = t1 then
                SomeE
                  (TTry
                     ( t0,
                       d0,
                       d1,
                       tp0,
                       tp1,
                       is_iso_mixed_expr_proof tp0
                       || is_iso_mixed_expr_proof tp1 ))
              else
                NoneE "Type mismatch in Try"
        | NoneE err, _
        | _, NoneE err ->
            NoneE (err ^ "\nin Try")
    end
  (* T-MIXEDAPP *)
  | Apply (f, e') -> begin
      match (mixed_type_check d e', prog_type_check f) with
      | SomeE tpe', SomeE tpf ->
          let t0, t1 = type_of_prog_proof tpf in
          let t' = type_of_mixed_expr_proof tpe' in
          let tpf =
            match tpf with
            | PureProg p -> TChannel p
            | MixedProg p -> p
          in
            if t' = t0 then
              SomeE
                (TMixedApp
                   ( t0,
                     t1,
                     d,
                     tpf,
                     tpe',
                     is_iso_mixed_prog_proof tpf
                     && is_iso_mixed_expr_proof tpe' ))
            else
              NoneE "Type mismatch in mixed Apply"
      | NoneE err, _
      | _, NoneE err ->
          NoneE (err ^ "\nin mixed Apply")
    end
  | _ -> begin
      match pure_type_check StringMap.empty d e with
      | SomeE tp -> SomeE (TMix tp)
      | NoneE err -> NoneE (err ^ "\nin Mix")
    end

(*
Finds the type of a pure expression e with a classical context g and a
quantum context d
*)
and pure_type_check (g : context) (d : context) (e : expr) :
    pure_expr_typing_proof optionE =
  match e with
  (* T-UNIT *)
  | Null -> begin
      match StringMap.bindings d with
      | [] -> SomeE (TUnit g)
      | _ -> NoneE "Irrelevant variables in quantum context in Null"
    end
  | Var x -> begin
      match StringMap.bindings d with
      | [] -> begin
          (* T-CVAR *)
          match StringMap.find_opt x g with
          | Some t -> SomeE (TCvar (t, g, x))
          | None -> NoneE (Printf.sprintf "Unbound variable %s in Var" x)
        end
      | [(x', t)] ->
          if x' <> x then
            NoneE
              (Printf.sprintf
                 "Irrelevant variable %s in quantum context in Var" x')
          else begin (* T-QVAR *)
            match StringMap.find_opt x g with
            | Some _ ->
                NoneE
                  (Printf.sprintf
                     "Variable %s appears in both classical and quantum \
                      context in Var"
                     x)
            | None -> SomeE (TQvar (t, d, x))
          end
      | _ -> NoneE "Irrelevant variables in quantum context in Var"
    end
  (* T-PUREPAIR *)
  | Qpair (e0, e1) -> begin
      let fv0 = free_vars e0 in
      let fv1 = free_vars e1 in
        if
          StringSet.diff (map_dom d)
            (StringSet.union (free_vars e0) (free_vars e1))
          <> StringSet.empty
        then
          NoneE "Irrelevant variables in quantum context in Qpair"
        else
          let d', d_xor = map_partition d (StringSet.inter fv0 fv1) in
          let d0, d1 = map_partition d_xor fv0 in
            match
              ( pure_type_check g (map_restriction d (free_vars e0)) e0,
                pure_type_check g (map_restriction d (free_vars e1)) e1 )
            with
            | SomeE tp0, SomeE tp1 ->
                SomeE
                  (TPurePair
                     ( type_of_pure_expr_proof tp0,
                       type_of_pure_expr_proof tp1,
                       g,
                       d',
                       d0,
                       d1,
                       tp0,
                       tp1,
                       is_iso_pure_expr_proof tp0 && is_iso_pure_expr_proof tp1
                     ))
            | NoneE err, _
            | _, NoneE err ->
                NoneE (err ^ "\nin Qpair")
    end
  (* T-CTRL *)
  | Ctrl (e', t0, l, t1) -> begin
      let l = List.sort (fun (e0, _) (e1, _) -> expr_compare e0 e1) l in
      let fve' = free_vars e' in
      let g0, g' = map_partition g fve' in
      let d0, d' = map_partition d fve' in
      let ej, ej' = List.split l in
        match map_merge false g0 d0 with
        | NoneE err -> NoneE (err ^ "\nin Ctrl")
        | SomeE g0d0 -> begin
            match mixed_type_check g0d0 e' with
            | SomeE tpe' when type_of_mixed_expr_proof tpe' = t0 -> begin
                match ortho_check t0 ej with
                | None -> NoneE "Ortho check failed in Ctrl"
                | Some orp -> begin
                    match
                      map_all_or_nothing
                        (StringMap.mapi (fun x _ -> erases_check x ej' t1) d0)
                    with
                    | None -> NoneE "Erasure check failed in Ctrl"
                    | Some erp -> begin
                        match
                          all_or_nothing
                            (List.map (pattern_type_check g d t0 t1) l)
                        with
                        | Some l' ->
                            let iso =
                              expr_is_classical e'
                              && is_iso_mixed_expr_proof tpe'
                              && is_spanning_ortho_proof orp
                              && List.for_all
                                   (fun (_, _, pf) ->
                                     is_iso_pure_expr_proof pf)
                                   l'
                            in
                              SomeE
                                (TCtrl
                                   ( t0,
                                     t1,
                                     g0,
                                     g',
                                     d0,
                                     d',
                                     tpe',
                                     l',
                                     orp,
                                     erp,
                                     iso ))
                        | _ -> NoneE "Type mismatch in Ctrl"
                      end
                  end
              end
            | _ -> NoneE "Type mismatch in Ctrl"
          end
    end
  | Try _ -> NoneE "Try is not a pure expression"
  (* T-PUREAPP *)
  | Apply (f, e') -> begin
      match (pure_type_check g d e', prog_type_check f) with
      | SomeE tpe', SomeE tpf -> begin
          match (tpf, type_of_prog_proof tpf) with
          | MixedProg _, _ ->
              NoneE "Attempted pure application of mixed program"
          | PureProg tpf, (t0, t1) -> begin
              let t' = type_of_pure_expr_proof tpe' in
                if t' = t0 then
                  SomeE
                    (TPureApp
                       ( t0,
                         t1,
                         g,
                         d,
                         tpf,
                         tpe',
                         is_iso_pure_prog_proof tpf
                         && is_iso_pure_expr_proof tpe' ))
                else
                  NoneE "Type mismatch in Apply"
            end
        end
      | NoneE err, _
      | _, NoneE err ->
          NoneE (err ^ "\nin Apply")
    end

(*
Given a mixed expression e expected to be of type t,
find the quantum context of the expression.
*)
and mixed_context_check (t : exprtype) (e : expr) : context optionE =
  match (e, t) with
  (* T-MIXEDPAIR *)
  | Qpair (e0, e1), ProdType (t0, t1) -> begin
      match (mixed_context_check t0 e0, mixed_context_check t1 e1) with
      | SomeE d0, SomeE d1 -> map_merge true d0 d1
      | NoneE err, _
      | _, NoneE err ->
          NoneE (err ^ "\nin mixed Qpair")
    end
  | Qpair _, _ -> NoneE "Expected product type in mixed Qpair"
  (* T-TRY *)
  | Try (e0, e1), _ -> begin
      match (mixed_context_check t e0, mixed_context_check t e1) with
      | SomeE d0, SomeE d1 -> map_merge false d0 d1
      | NoneE err, _
      | _, NoneE err ->
          NoneE (err ^ "\nin Try")
    end
  (* T-MIXEDAPP *)
  | Apply (f, e'), _ -> begin
      match prog_type_check f with
      | SomeE tpf -> begin
          let t0, t1 = type_of_prog_proof tpf in
            if t = t1 then
              mixed_context_check t0 e'
            else
              NoneE "Type mismatch in mixed Apply"
        end
      | NoneE err -> NoneE (err ^ "\nin mixed Apply")
    end
  | _, _ -> context_check StringMap.empty t e

(*
Given a pure expression e expected to be of type t with classical context g,
find the quantum context of the expression.
*)
and context_check (g : context) (t : exprtype) (e : expr) : context optionE =
  match (e, t) with
  (* T-UNIT *)
  | Null, Qunit -> SomeE StringMap.empty
  | Var x, _ -> begin
      match StringMap.find_opt x g with
      (* T-CVAR *)
      | Some t' ->
          if t = t' then
            SomeE StringMap.empty
          else
            NoneE "Type mismatch in classical context in Var"
      (* T-QVAR *)
      | None -> SomeE (StringMap.singleton x t)
    end
  (* T-PUREPAIR *)
  | Qpair (e0, e1), ProdType (t0, t1) -> begin
      match (context_check g t0 e0, context_check g t1 e1) with
      | SomeE d0, SomeE d1 -> map_merge true d0 d1
      | NoneE err, _
      | _, NoneE err ->
          NoneE (err ^ "\nin Qpair")
    end
  (* T-CTRL *)
  | Ctrl (e', t0, l, t1), _ -> begin
      match (mixed_context_check t0 e', l) with
      | NoneE err, _ -> NoneE (err ^ "\nin Ctrl")
      | SomeE d, [] -> SomeE d
      | SomeE d, (e0, e0') :: _ -> begin
          match first_pattern_context_check g t0 t1 e0 e0' with
          | NoneE err -> NoneE (err ^ "\nin Ctrl")
          | SomeE d0 -> begin
              (* d is Γ, Δ
                 g is Γ, Γ'
                 d0 is Δ, Δ' *)
              match map_merge false g d0 with
              | NoneE err -> NoneE (err ^ "\nin Ctrl")
              | SomeE gd0 ->
                  let ej, ej' = List.split l in
                    if not (map_inclusion d gd0) then
                      NoneE "Context inclusion failed in Ctrl"
                    else if ortho_check t0 ej = None then
                      NoneE "Ortho check failed in Ctrl"
                    else if
                      not
                        (List.for_all
                           (fun x -> pattern_type_check g d0 t0 t1 x <> None)
                           l)
                    then
                      NoneE "Type mismatch in Ctrl"
                    else if
                      not
                        (StringMap.for_all
                           (fun x _ -> erases_check x ej' t1 <> None)
                           d)
                    then
                      NoneE "Erasure check failed in Ctrl"
                    else
                      SomeE d0
            end
        end
    end
  | Try _, _ -> NoneE "Try is not a pure expression"
  (* T-PUREAPP *)
  | Apply (f, e'), _ -> begin
      match prog_type_check f with
      | SomeE (PureProg tpf) -> begin
          let t0, t1 = type_of_prog_proof (PureProg tpf) in
            if t = t1 then
              context_check g t0 e'
            else
              NoneE "Type mismatch in pure Apply"
        end
      | SomeE (MixedProg _) ->
          NoneE "Attempted pure application of mixed program"
      | NoneE err -> NoneE (err ^ "\nin Apply")
    end
  | _, _ -> NoneE "Type mismatch"

and prog_type_check (f : prog) : prog_typing_proof optionE =
  match f with
  (* T-GATE *)
  | U3 (theta, phi, lambda) -> SomeE (PureProg (TGate (theta, phi, lambda)))
  (* T-LEFT *)
  | Left (t0, t1) -> SomeE (PureProg (TLeft (t0, t1)))
  (* T-RIGHT *)
  | Right (t0, t1) -> SomeE (PureProg (TRight (t0, t1)))
  | Lambda (e, t, e') -> begin
      let is_e_spanning =
        begin
          match missing_span t [e] with
          | Some ([], _) -> true
          | _ -> false
        end
      in
        match context_check StringMap.empty t e with
        | NoneE err -> NoneE (err ^ "\nin Lambda")
        | SomeE d -> begin
            match
              ( pure_type_check StringMap.empty d e,
                pure_type_check StringMap.empty d e' )
            with
            (* T-PUREABS *)
            | SomeE tpe, SomeE tpe' ->
                let iso = is_iso_pure_expr_proof tpe' && is_e_spanning in
                  SomeE
                    (PureProg
                       (TPureAbs
                          (t, type_of_pure_expr_proof tpe', d, tpe, tpe', iso)))
            | _ -> begin
                let d', d0 = map_partition d (free_vars e') in
                  match
                    ( pure_type_check StringMap.empty d e,
                      mixed_type_check d' e' )
                  with
                  | NoneE err, _
                  | _, NoneE err ->
                      NoneE (err ^ "\nin Lambda")
                  (* T-MIXEDABS *)
                  | SomeE tpe, SomeE tpe' ->
                      let iso =
                        is_iso_mixed_expr_proof tpe' && is_e_spanning
                      in
                        SomeE
                          (MixedProg
                             (TMixedAbs
                                ( t,
                                  type_of_mixed_expr_proof tpe',
                                  d',
                                  d0,
                                  tpe,
                                  tpe',
                                  iso )))
              end
          end
    end
  (* T-RPHASE *)
  | Rphase (t, e', r, r') -> begin
      match context_check StringMap.empty t e' with
      | NoneE err -> NoneE (err ^ "\nin Rphase")
      | SomeE d -> begin
          match pure_type_check StringMap.empty d e' with
          | NoneE err -> NoneE (err ^ "\nin Rphase")
          | SomeE tpe' when type_of_pure_expr_proof tpe' = t ->
              SomeE (PureProg (TRphase (t, tpe', r, r')))
          | _ -> NoneE "Type mismatch in Rphase"
        end
    end

let pure_type_check_noopt (e : expr) : pure_expr_typing_proof =
  match pure_type_check StringMap.empty StringMap.empty e with
  | SomeE t -> t
  | NoneE err -> failwith err

let mixed_type_check_noopt (e : expr) : mixed_expr_typing_proof =
  match mixed_type_check StringMap.empty e with
  | SomeE t -> t
  | NoneE err -> failwith err

let prog_type_check_noopt (f : prog) : prog_typing_proof =
  match prog_type_check f with
  | SomeE ft -> ft
  | NoneE err -> failwith err
