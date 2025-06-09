open Util
open Reals
open Syntax

(** A data structure representing a pure expression typing judgment proof. *)
type pure_expr_typing_proof =
  | TUnit of context
  | TCvar of { t : exprtype; g : context; x : string }
  | TQvar of { t : exprtype; g : context; d : context; x : string }
  | TPurePair of {
      t0 : exprtype;
      t1 : exprtype;
      g : context;
      d : context;
      d0 : context;
      d1 : context;
      e0 : pure_expr_typing_proof;
      e1 : pure_expr_typing_proof;
      iso : bool;
    }
  | TCtrl of {
      t0 : exprtype;
      t1 : exprtype;
      g : context;
      d : context;
      d' : context;
      e : mixed_expr_typing_proof;
      l : (context * pure_expr_typing_proof * pure_expr_typing_proof) list;
      orp : ortho_proof;
      erp : erasure_proof StringMap.t;
      iso : bool;
    }
  | TPureApp of {
      t : exprtype;
      t' : exprtype;
      g : context;
      d : context;
      f : pure_prog_typing_proof;
      e : pure_expr_typing_proof;
      iso : bool;
    }

(** A data structure representing a mixed expression typing judgment proof. *)
and mixed_expr_typing_proof =
  | TMix of pure_expr_typing_proof
  | TDiscard of {
      t : exprtype;
      g : context;
      d : context;
      d0 : context;
      e : mixed_expr_typing_proof;
      iso : bool;
    }
  | TMixedPair of {
      t0 : exprtype;
      t1 : exprtype;
      g : context;
      d : context;
      d0 : context;
      d1 : context;
      e0 : mixed_expr_typing_proof;
      e1 : mixed_expr_typing_proof;
      iso : bool;
    }
  | TMatch of {
      t0 : exprtype;
      t1 : exprtype;
      g : context;
      d : context;
      d0 : context;
      d1 : context;
      e : mixed_expr_typing_proof;
      l : (context * pure_expr_typing_proof * mixed_expr_typing_proof) list;
      orp : ortho_proof;
      iso : bool;
    }
  | TTry of {
      t : exprtype;
      g : context;
      d0 : context;
      d1 : context;
      e0 : mixed_expr_typing_proof;
      e1 : mixed_expr_typing_proof;
      iso : bool;
    }
  | TMixedApp of {
      t : exprtype;
      t' : exprtype;
      g : context;
      d : context;
      f : mixed_prog_typing_proof;
      e : mixed_expr_typing_proof;
      iso : bool;
    }

(** A data structure representing a pure program typing judgment proof. *)
and pure_prog_typing_proof =
  | TGate of real * real * real
  | TLeft of exprtype * exprtype
  | TRight of exprtype * exprtype
  | TPureAbs of {
      t : exprtype;
      t' : exprtype;
      d : context;
      e : pure_expr_typing_proof;
      e' : pure_expr_typing_proof;
      iso : bool;
    }
  | TRphase of {
      t : exprtype;
      e : pure_expr_typing_proof;
      r0 : real;
      r1 : real;
      iso : bool;
    }
  | TPmatch of {
      t0 : exprtype;
      t1 : exprtype;
      l : (context * pure_expr_typing_proof * pure_expr_typing_proof) list;
      orp0 : ortho_proof;
      orp1 : ortho_proof;
      perm0 : int list;
      perm1 : int list;
      iso : bool;
    }

(** A data structure representing a mixed program typing judgment proof. *)
and mixed_prog_typing_proof =
  | TChannel of pure_prog_typing_proof
  | TMixedAbs of {
      t : exprtype;
      t' : exprtype;
      d : context;
      e : pure_expr_typing_proof;
      e' : mixed_expr_typing_proof;
      iso : bool;
    }

(** A data structure representing a pure or mixed program typing judgment
    proof. *)
and prog_typing_proof =
  | PureProg of pure_prog_typing_proof
  | MixedProg of mixed_prog_typing_proof

(** A data structure representing an erasure judgment proof. *)
and erasure_proof =
  | EVar of exprtype
  | EPair0 of exprtype * exprtype * erasure_proof
  | EPair1 of exprtype * exprtype * erasure_proof

(** A data structure representing a spanning judgment proof. *)
and spanning_proof =
  | SVoid
  | SUnit
  | SUnApp of pure_prog_typing_proof * spanning_proof
  | SVar of string * exprtype
  | SSum of spanning_proof * spanning_proof
  | SPair of exprtype * exprtype * spanning_proof * spanning_proof list

and ortho_proof = spanning_proof * bool list
(** A data structure representing an orthogonality judgment proof. *)

(** Obtains the expression type from a pure expression typing judgment proof.
*)
let type_of_pure_expr_proof (tp : pure_expr_typing_proof) : exprtype =
  match tp with
  | TUnit _ -> Qunit
  | TCvar { t; _ } -> t
  | TQvar { t; _ } -> t
  | TPurePair { t0; t1; _ } -> ProdType (t0, t1)
  | TCtrl { t1; _ } -> t1
  | TPureApp { t'; _ } -> t'

(** Obtains the expression type from a mixed expression typing judgment proof.
*)
let type_of_mixed_expr_proof (tp : mixed_expr_typing_proof) : exprtype =
  match tp with
  | TMix p -> type_of_pure_expr_proof p
  | TDiscard { t; _ } -> t
  | TMixedPair { t0; t1; _ } -> ProdType (t0, t1)
  | TMatch { t1; _ } -> t1
  | TTry { t; _ } -> t
  | TMixedApp { t'; _ } -> t'

(** Obtains the input and output expression types from a program typing
    judgment proof. *)
let rec type_of_prog_proof (tp : prog_typing_proof) : exprtype * exprtype =
  match tp with
  | PureProg (TGate _) -> (bit, bit)
  | PureProg (TLeft (t0, t1)) -> (t0, SumType (t0, t1))
  | PureProg (TRight (t0, t1)) -> (t1, SumType (t0, t1))
  | PureProg (TPureAbs { t; t'; _ }) -> (t, t')
  | PureProg (TRphase { t; _ }) -> (t, t)
  | PureProg (TPmatch { t0; t1; _ }) -> (t0, t1)
  | MixedProg (TChannel tp') -> type_of_prog_proof (PureProg tp')
  | MixedProg (TMixedAbs { t; t'; _ }) -> (t, t')

(** Obtains the program type from a program typing judgment proof. *)
let progtype_of_prog_proof (tp : prog_typing_proof) : progtype =
  let t, t' = type_of_prog_proof tp in
    match tp with
    | PureProg _ -> Coherent (t, t')
    | MixedProg _ -> Channel (t, t')

(** Obtains the classical and quantum contexts from a pure expression typing
    judgment proof. *)
let context_of_pure_expr_proof (tp : pure_expr_typing_proof) :
    context * context =
  match tp with
  | TUnit g -> (g, StringMap.empty)
  | TCvar { g; _ } -> (g, StringMap.empty)
  | TQvar { g; d; _ } -> (g, d)
  | TPurePair { g; d; d0; d1; _ } ->
      (g, map_merge_noopt false d (map_merge_noopt false d0 d1))
  | TCtrl { g; d; d'; _ } -> (g, map_merge_noopt false d d')
  | TPureApp { g; d; _ } -> (g, d)

(** Obtains the quantum context from a mixed expression typing judgment proof.
*)
let context_of_mixed_expr_proof (tp : mixed_expr_typing_proof) :
    context * context =
  match tp with
  | TMix tp' -> context_of_pure_expr_proof tp'
  | TDiscard { g; d; d0; _ } -> (g, map_merge_noopt false d d0)
  | TMixedPair { g; d; d0; d1; _ } ->
      (g, map_merge_noopt false d (map_merge_noopt false d0 d1))
  | TMatch { g; d; d0; d1; _ } ->
      (g, map_merge_noopt false d (map_merge_noopt false d0 d1))
  | TTry { g; d0; d1; _ } -> (g, map_merge_noopt false d0 d1)
  | TMixedApp { g; d; _ } -> (g, d)

(** Obtains the isometry judgment information from a pure expression typing
    judgment proof. *)
let is_iso_pure_expr_proof (tp : pure_expr_typing_proof) : bool =
  match tp with
  | TUnit _
  | TCvar _
  | TQvar _ ->
      true
  | TPurePair { iso; _ }
  | TCtrl { iso; _ }
  | TPureApp { iso; _ } ->
      iso

(** Obtains the isometry judgment information from a mixed expression typing
    judgment proof. *)
let is_iso_mixed_expr_proof (tp : mixed_expr_typing_proof) : bool =
  match tp with
  | TMix tp' -> is_iso_pure_expr_proof tp'
  | TDiscard { iso; _ }
  | TMixedPair { iso; _ }
  | TMatch { iso; _ }
  | TTry { iso; _ }
  | TMixedApp { iso; _ } ->
      iso

(** Obtains the isometry judgment information from a pure program typing
    judgment proof. *)
let is_iso_pure_prog_proof (tp : pure_prog_typing_proof) : bool =
  match tp with
  | TGate _
  | TLeft _
  | TRight _ ->
      true
  | TRphase { iso; _ }
  | TPureAbs { iso; _ }
  | TPmatch { iso; _ } ->
      iso

(** Obtains the isometry judgment information from a mixed program typing
    judgment proof. *)
let is_iso_mixed_prog_proof (tp : mixed_prog_typing_proof) : bool =
  match tp with
  | TChannel tp' -> is_iso_pure_prog_proof tp'
  | TMixedAbs { iso; _ } -> iso

(** Obtains the isometry judgment information from a program typing judgment
    proof. *)
let is_iso_prog_proof (tp : prog_typing_proof) : bool =
  match tp with
  | PureProg tp' -> is_iso_pure_prog_proof tp'
  | MixedProg tp' -> is_iso_mixed_prog_proof tp'

(** Obtains the unitary judgment information from a pure expression typing
    judgment proof. *)
let is_un_pure_expr_proof (tp : pure_expr_typing_proof) : bool =
  match tp with
  | TUnit _
  | TQvar _ ->
      true
  | TCvar _ -> false
  | TPurePair { iso; _ }
  | TCtrl { iso; _ }
  | TPureApp { iso; _ } ->
      let g, d = context_of_pure_expr_proof tp in
      let t = type_of_pure_expr_proof tp in
        iso && g = StringMap.empty && context_dimension d = type_dimension t

(** Obtains the unitary judgment information from a pure program typing
    judgment proof. *)
let is_un_pure_prog_proof (tp : pure_prog_typing_proof) : bool =
  match tp with
  | TGate _ -> true
  | TLeft _
  | TRight _ ->
      false
  | TRphase { iso; _ }
  | TPureAbs { iso; _ }
  | TPmatch { iso; _ } ->
      let t, t' = type_of_prog_proof (PureProg tp) in
        iso && type_dimension t = type_dimension t'

(** Determines if an expression is classical (does not use [U3] or [Rphase]).
*)
let rec expr_is_classical (e : expr) : bool =
  match e with
  | Null
  | Var _ ->
      true
  | Qpair (e0, e1) -> expr_is_classical e0 && expr_is_classical e1
  | Ctrl (e', _, _, l)
  | Match (e', _, _, l) ->
      expr_is_classical e'
      && List.for_all
           (fun (ej, ej') -> expr_is_classical ej && expr_is_classical ej')
           l
  | Try (e0, e1) -> expr_is_classical e0 && expr_is_classical e1
  | Apply (f, e') -> prog_is_classical f && expr_is_classical e'

(** Determines if a program is classical (does not use [U3] or [Rphase]). *)
and prog_is_classical (f : prog) : bool =
  match f with
  | U3 _
  | Rphase _ ->
      false
  | Left _
  | Right _ ->
      true
  | Lambda (e0, _, e1) -> expr_is_classical e0 && expr_is_classical e1
  | Pmatch (_, _, l) ->
      List.for_all
        (fun (ej, ej') -> expr_is_classical ej && expr_is_classical ej')
        l

(** Checks if an orthogonality proof directly corresponds to a spanning proof
    (if the corresponding expressions form a full spanning set). *)
let is_spanning_ortho_proof (orp : ortho_proof) : bool =
  let _, selection = orp in
    List.for_all (fun x -> x) selection

(** Finds the free variables in an expression e. *)
let rec free_vars (e : expr) : StringSet.t =
  match e with
  | Null -> StringSet.empty
  | Var x -> StringSet.singleton x
  | Qpair (e1, e2)
  | Try (e1, e2) ->
      StringSet.union (free_vars e1) (free_vars e2)
  | Ctrl (e', _, _, l)
  | Match (e', _, _, l) ->
      List.fold_right
        (fun (ej, ej') rest ->
          (* The free variables of the left-hand side patterns in a control
             block are not considered free variables in the expression. *)
          StringSet.union (StringSet.diff (free_vars ej') (free_vars ej)) rest)
        l (free_vars e')
  | Apply (_, e') -> free_vars e'

(** Removes global phases from an expression. *)
let rec dephase (e : expr) : expr =
  match e with
  | Apply (Rphase (_, _, r0, r1), e') when r0 = r1 -> dephase e'
  | _ -> e

(** Removes global phases from an expression, and in the case of a ctrl
    expression, returns its dephased right-hand-side expressions as a list. *)
let rec dephase_and_split_ctrl (e : expr) : expr list =
  match e with
  | Apply (Rphase (_, _, r0, r1), e') when r0 = r1 -> dephase_and_split_ctrl e'
  | Ctrl (_, _, _, l) ->
      List.flatten (List.map (fun (_, ej') -> dephase_and_split_ctrl ej') l)
  | _ -> [e]

(** Given a list [l] of Qpairs, splits the list into two lists. *)
let rec split_qpair_list (l : expr list) : (expr list * expr list) option =
  match l with
  | [] -> Some ([], [])
  | Qpair (e0, e1) :: l' -> begin
      match split_qpair_list l' with
      | Some (l0, l1) -> Some (e0 :: l0, e1 :: l1)
      | None -> None
    end
  | _ -> None

(** Checks whether the erasure check is satisfied, outputting an erasure
    judgment proof. *)
let rec erases_check (x : string) (l : expr list) (t : exprtype) :
    erasure_proof option =
  let l' = List.flatten (List.map dephase_and_split_ctrl l) in
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

(** Given a list l of expressions expected to be of type [SumType (t0, t1)],
    splits the list into two lists - one containing all the "left" expressions
    of type [t0] and one containing all the "right" exressions of type [t1]. *)
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

(** Adds a pair of expressions ([e0], [e1]) to a list consisting of pairs of
    expressions and expression lists. If [e0] occurs as one of these
    expressions, [e1] is added to its corresponding list. Otherwise, the pair
    consisting of [e0] and the singleton list [[e1]] is added to the list of
    pairs *)
let rec add_to_qpair_list (e0 : expr) (e1 : expr) (l : (expr * expr list) list)
    : (expr * expr list) list =
  match l with
  | [] -> [(e0, [e1])]
  | (e0', l1) :: l' ->
      if e0' = e0 then
        (e0', e1 :: l1) :: l'
      else
        (e0', l1) :: add_to_qpair_list e0 e1 l'

(** Converts a list of [Qpair]'s of expressions (of type [ProdType (t0, t1)])
    into a list associating each unique expression of type [t0] with a list of
    all expressions of type [t1] with which it appears in the original list. *)
let rec spread_qpair_list (l : expr list) : (expr * expr list) list option =
  match l with
  | [] -> Some []
  | Qpair (e0, e1) :: l' -> begin
      match spread_qpair_list l' with
      | Some l'' -> Some (add_to_qpair_list e0 e1 l'')
      | None -> None
    end
  | _ -> None

(** Comparison for two expressions (assumed to be of the same type). If the
    expressions can't both be on the left-hand side of a control, match, or
    pmatch block, the output is meaningless. *)
let rec expr_compare (e0 : expr) (e1 : expr) =
  match (e0, e1) with
  | Null, Null -> 0
  | Apply (Left _, _), Apply (Right _, _) -> -1
  | Apply (Right _, _), Apply (Left _, _) -> 1
  | Apply (f0, e0), Apply (f1, e1) when f0 = f1 -> expr_compare e0 e1
  | Qpair (e00, e01), Qpair (e10, e11) ->
      let c0 = expr_compare e00 e10 in
        if c0 = 0 then expr_compare e01 e11 else c0
  | _ -> 0

(** Checks if every element in the given list is an application of the same
    unitary program to something, and if so, returns the program, its typing
    proof, and the list with the applications removed. *)
let rec check_unapp (l : expr list) :
    (prog * pure_prog_typing_proof * expr list) option =
  match
    all_or_nothing
      (List.map
         (fun e ->
           match e with
           | Apply (f, e') -> Some (f, e')
           | _ -> None)
         l)
  with
  | None -> None
  | Some fs_l' -> begin
      let fs, l' = List.split fs_l' in
        if fs = [] then
          None
        else if not (List.for_all (fun f -> f = List.hd fs) fs) then
          None
        else
          let f = List.hd fs in
          let tpf = prog_type_check f in
            match tpf with
            | SomeE (PureProg tpf) ->
                Some (f, tpf, l')
                (* if is_un_pure_prog_proof tpf then Some (f, tpf, l') else None *)
            | _ -> None
    end

(** If possible, fills the missing span for a given list of expressions to make
    the spanning judgment hold. Also returns the corresponding spanning proof
    structure. *)
and missing_span (t : exprtype) (l : expr list) (allow_quantum : bool) :
    (expr list * spanning_proof) option =
  let rec missing_span_helper (t : exprtype) (l : expr list) (fv : StringSet.t)
      : (expr list * spanning_proof) option =
    match (t, l, check_unapp l) with
    | _, _, Some (f, tpf, l') when allow_quantum -> begin
        match
          missing_span_helper (fst (type_of_prog_proof (PureProg tpf))) l' fv
        with
        | None -> None
        | Some (l'', sp) ->
            Some (List.map (fun e -> Apply (f, e)) l'', SUnApp (tpf, sp))
      end
    | Void, [], _ -> Some ([], SVoid)
    | Qunit, [], _ -> Some ([Null], SUnit)
    | Qunit, [Null], _ -> Some ([], SUnit)
    | _, [], _ ->
        let x = fresh_string "&" fv in
          Some ([Var x], SVar (x, t))
    | _, [Var x], _ ->
        if StringSet.mem x fv then None else Some ([], SVar (x, t))
    | SumType (t0, t1), _, _ -> begin
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
                    SSum (sp0, sp1) )
            | _, _ -> None
          end
      end
    | ProdType (t0, t1), _, _ -> begin
        match spread_qpair_list l with
        | None -> None
        | Some l' -> begin
            match missing_span_helper t0 (List.map fst l') fv with
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
                             Some (List.map (fun e1 -> Qpair (e0, e1)) l1', tp1)
                         | None -> None)
                       l'')
                in
                  match result with
                  | Some r ->
                      Some
                        ( List.flatten (List.map fst r),
                          SPair (t0, t1, sp0, List.map snd r) )
                  | None -> None
              end
          end
      end
    | _, _, _ -> None
  in
    missing_span_helper t l StringSet.empty

(** Given an expression type [t] and list [l] of expressions, expected to be of
    type [t], extends the list to one "spanning" [t], if possible. *)
and span_list (t : exprtype) (l : expr list) (allow_quantum : bool) :
    expr list option =
  match missing_span t l allow_quantum with
  | Some (l', _) -> Some (List.sort expr_compare (l @ l'))
  | None -> None

(** Checks if the given expression list [l] satisfies the orthogonality
    judgment, and if so, outputs an orthogonality judgment proof data
    structure. *)
and ortho_check (t : exprtype) (l : expr list) (allow_quantum : bool) :
    ortho_proof option =
  match missing_span t (List.map dephase l) allow_quantum with
  | Some (l', sp) ->
      let span_list = List.sort expr_compare (l @ l') in
      let ortho_list = List.sort expr_compare l in
      let selection = List.map (fun e -> List.mem e ortho_list) span_list in
        Some (sp, selection)
  | None -> None

(** In T-CTRL, given a classical context [g] and a pair [ej], [ej'], the
    quantum context of [ej] has to be some [gj] with no classical context -
    then, with the classical context obtained by merging [g] and [gj], this
    finds the quantum context of [ej']. *)
and first_pattern_context_check (g : context) (t : exprtype) (t' : exprtype)
    (ej : expr) (ej' : expr) : context optionE =
  match pure_context_check StringMap.empty t ej with
  | NoneE err -> NoneE err
  | SomeE gj -> begin
      match map_merge false g gj with
      | SomeE ggj -> pure_context_check ggj t' ej'
      | NoneE err -> NoneE err
    end

(** In T-CTRL, checks the type of an expression on the right-hand side of a
    control block under the contexts obtained from the first pattern in the
    block. *)
and pure_pattern_type_check (g : context) (d : context) (t : exprtype)
    (t' : exprtype) ((ej, ej') : expr * expr) :
    (context * pure_expr_typing_proof * pure_expr_typing_proof) optionE =
  match pure_context_check StringMap.empty t ej with
  | NoneE err -> NoneE err
  | SomeE gj -> begin
      match map_merge false g gj with
      | SomeE ggj -> begin
          match
            (pure_type_check StringMap.empty gj ej, pure_type_check ggj d ej')
          with
          | SomeE tpej, SomeE tpej' ->
              if type_of_pure_expr_proof tpej' = t' then
                SomeE (gj, tpej, tpej')
              else
                NoneE "Type mismatch in pattern type check"
          | NoneE err, _
          | _, NoneE err ->
              NoneE err
        end
      | NoneE err -> NoneE err
    end

(** In T-MATCH, checks the type of an expression on the right-hand side of a
    control block under the contexts obtained from the first pattern in the
    block. *)
and mixed_pattern_type_check (g : context) (d : context) (t : exprtype)
    (t' : exprtype) ((ej, ej') : expr * expr) :
    (context * pure_expr_typing_proof * mixed_expr_typing_proof) optionE =
  match pure_context_check StringMap.empty t ej with
  | NoneE err -> NoneE err
  | SomeE gj -> begin
      match map_merge false g gj with
      | SomeE ggj -> begin
          match
            (pure_type_check StringMap.empty gj ej, mixed_type_check ggj d ej')
          with
          | SomeE tpej, SomeE tpej' ->
              if type_of_mixed_expr_proof tpej' = t' then
                SomeE (gj, tpej, tpej')
              else
                NoneE "Type mismatch in pattern type check"
          | NoneE err, _
          | _, NoneE err ->
              NoneE err
        end
      | NoneE err -> NoneE err
    end

(** In T-MATCH, finds the quantum context shared by all the RHS expressions. *)
and match_dd1_context_check (g : context) (t0 : exprtype) (t1 : exprtype)
    (l : (expr * expr) list) : context optionE =
  match
    all_or_nothing_optionE
      (List.map
         (fun (ej, ej') ->
           begin
             match pure_context_check StringMap.empty t0 ej with
             | SomeE gj -> begin
                 match mixed_context_check gj t1 ej' with
                 | SomeE dj -> SomeE dj
                 | NoneE err -> NoneE err
               end
             | NoneE err -> NoneE err
           end)
         l)
  with
  | NoneE err -> NoneE err
  | SomeE l ->
      SomeE
        (map_exclusion
           (List.fold_left (map_merge_noopt true) StringMap.empty l)
           (map_dom g))

(** Typechecks a mixed expression [e] with a classical context [g] and a
    quantum context [d]. *)
and mixed_type_check (g : context) (d : context) (e : expr) :
    mixed_expr_typing_proof optionE =
  let error_addition =
    Printf.sprintf "\n\nin %s\ntyped as a mixed expression with g = %s, d = %s"
      (string_of_expr e) (string_of_context g) (string_of_context d)
  in
    match e with
    (* T-DISCARD *)
    | _ when map_exclusion d (free_vars e) <> StringMap.empty -> begin
        let d, d0 = map_partition d (free_vars e) in
          match mixed_type_check g d e with
          | SomeE tpe ->
              SomeE
                (TDiscard
                   {
                     t = type_of_mixed_expr_proof tpe;
                     g;
                     d;
                     d0;
                     e = tpe;
                     iso = is_iso_mixed_expr_proof tpe;
                   })
          | NoneE err -> NoneE err
      end
    (* T-MIXEDPAIR *)
    | Qpair (e0, e1) -> begin
        let fv0 = free_vars e0 in
        let fv1 = free_vars e1 in
        let irrelevant =
          StringSet.to_list
            (StringSet.diff (map_dom d)
               (StringSet.union (free_vars e0) (free_vars e1)))
        in
          if irrelevant <> [] then
            NoneE
              (Printf.sprintf
                 "Irrelevant variables %s in quantum context in mixed Qpair"
                 (string_of_list (fun x -> x) irrelevant)
              ^ error_addition)
          else
            let d', d_xor = map_partition d (StringSet.inter fv0 fv1) in
            let d0, d1 = map_partition d_xor fv0 in
              match
                ( mixed_type_check g (map_restriction d (free_vars e0)) e0,
                  mixed_type_check g (map_restriction d (free_vars e1)) e1 )
              with
              | SomeE tp0, SomeE tp1 ->
                  SomeE
                    (TMixedPair
                       {
                         t0 = type_of_mixed_expr_proof tp0;
                         t1 = type_of_mixed_expr_proof tp1;
                         g;
                         d = d';
                         d0;
                         d1;
                         e0 = tp0;
                         e1 = tp1;
                         iso =
                           is_iso_mixed_expr_proof tp0
                           && is_iso_mixed_expr_proof tp1;
                       })
              | NoneE err, _
              | _, NoneE err ->
                  NoneE (err ^ error_addition)
      end
    (* T-MATCH *)
    | Match (e', t0, t1, l) -> begin
        let l = List.sort (fun (e0, _) (e1, _) -> expr_compare e0 e1) l in
        let fve' = free_vars e' in
        let ejs, _ = List.split l in
        let dd0, d1 = map_partition d fve' in
        let dd1 = match_dd1_context_check g t0 t1 l in
          match (mixed_type_check g dd0 e', dd1) with
          | SomeE tpe', SomeE dd1 -> begin
              if type_of_mixed_expr_proof tpe' <> t0 then
                NoneE ("Type mismatch" ^ error_addition)
              else
                let d = map_exclusion dd1 (map_dom d1) in
                let d0 = map_exclusion dd0 (map_dom d) in
                  match ortho_check t0 ejs false with
                  | None -> NoneE ("Ortho check failed" ^ error_addition)
                  | Some orp -> begin
                      let pattern_result =
                        List.map (mixed_pattern_type_check g dd1 t0 t1) l
                      in
                        match
                          all_or_nothing
                            (List.map option_of_optionE pattern_result)
                        with
                        | Some l' ->
                            let iso =
                              expr_is_classical e'
                              && is_iso_mixed_expr_proof tpe'
                              && is_spanning_ortho_proof orp
                              && List.for_all
                                   (fun (_, _, tpej') ->
                                     is_iso_mixed_expr_proof tpej')
                                   l'
                            in
                              SomeE
                                (TMatch
                                   {
                                     t0;
                                     t1;
                                     g;
                                     d;
                                     d0;
                                     d1;
                                     e = tpe';
                                     l = l';
                                     orp;
                                     iso;
                                   })
                        | _ -> begin
                            match
                              List.find
                                (fun x -> option_of_optionE x = None)
                                pattern_result
                            with
                            | NoneE err -> NoneE (err ^ error_addition)
                            | _ -> failwith "unreachable"
                          end
                    end
            end
          | NoneE err, _
          | _, NoneE err ->
              NoneE (err ^ error_addition)
      end
    (* T-TRY *)
    | Try (e0, e1) -> begin
        let d0, d1 = map_partition d (free_vars e0) in
          match (mixed_type_check g d0 e0, mixed_type_check g d1 e1) with
          | SomeE tp0, SomeE tp1 ->
              let t0 = type_of_mixed_expr_proof tp0 in
              let t1 = type_of_mixed_expr_proof tp1 in
                if t0 = t1 then
                  SomeE
                    (TTry
                       {
                         t = t0;
                         g;
                         d0;
                         d1;
                         e0 = tp0;
                         e1 = tp1;
                         iso =
                           is_iso_mixed_expr_proof tp0
                           || is_iso_mixed_expr_proof tp1;
                       })
                else
                  NoneE ("Type mismatch" ^ error_addition)
          | NoneE err, _
          | _, NoneE err ->
              NoneE (err ^ error_addition)
      end
    (* T-MIXEDAPP *)
    | Apply (f, e') -> begin
        match (mixed_type_check g d e', prog_type_check f) with
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
                     {
                       t = t0;
                       t' = t1;
                       g;
                       d;
                       f = tpf;
                       e = tpe';
                       iso =
                         is_iso_mixed_prog_proof tpf
                         && is_iso_mixed_expr_proof tpe';
                     })
              else
                NoneE ("Type mismatch" ^ error_addition)
        | NoneE err, _
        | _, NoneE err ->
            NoneE (err ^ error_addition)
      end
    | _ -> begin
        match pure_type_check g d e with
        | SomeE tp -> SomeE (TMix tp)
        | NoneE err -> NoneE (err ^ error_addition)
      end

(** Typechecks a pure expression [e] with a classical context [g] and a quantum
    context [d]. *)
and pure_type_check (g : context) (d : context) (e : expr) :
    pure_expr_typing_proof optionE =
  let error_addition =
    Printf.sprintf "\n\nin %s\ntyped as a pure expression with g = %s, d = %s"
      (string_of_expr e) (string_of_context g) (string_of_context d)
  in
    match e with
    (* T-UNIT *)
    | Null -> begin
        match StringMap.bindings d with
        | [] -> SomeE (TUnit g)
        | l ->
            NoneE
              (Printf.sprintf "Irrelevant variables %s in quantum context"
                 (string_of_list (fun x -> x) (List.map fst l))
              ^ error_addition)
      end
    | Var x -> begin
        match StringMap.bindings d with
        | [] -> begin
            (* T-CVAR *)
            match StringMap.find_opt x g with
            | Some t -> SomeE (TCvar { t; g; x })
            | None ->
                NoneE (Printf.sprintf "Unbound variable %s" x ^ error_addition)
          end
        | [(x', t)] ->
            if x' <> x then
              NoneE
                (Printf.sprintf "Irrelevant variable %s in quantum context" x'
                ^ error_addition)
            else begin
              (* T-QVAR *)
                match StringMap.find_opt x g with
              | Some _ ->
                  NoneE
                    (Printf.sprintf
                       "Variable %s appears in both classical and quantum \
                        context"
                       x
                    ^ error_addition)
              | None -> SomeE (TQvar { t; g; d; x })
            end
        | l ->
            NoneE
              (Printf.sprintf "Irrelevant variables %s in quantum context"
                 (string_of_list (fun x -> x) (List.map fst l))
              ^ error_addition)
      end
    (* T-PUREPAIR *)
    | Qpair (e0, e1) -> begin
        let fv0 = free_vars e0 in
        let fv1 = free_vars e1 in
        let irrelevant =
          StringSet.to_list
            (StringSet.diff (map_dom d)
               (StringSet.union (free_vars e0) (free_vars e1)))
        in
          if irrelevant <> [] then
            NoneE
              (Printf.sprintf "Irrelevant variables %s in quantum context"
                 (string_of_list (fun x -> x) irrelevant)
              ^ error_addition)
          else
            let d', d_xor = map_partition d (StringSet.inter fv0 fv1) in
            let d0, d1 = map_partition d_xor fv0 in
              match
                ( pure_type_check g (map_restriction d (free_vars e0)) e0,
                  pure_type_check g (map_restriction d (free_vars e1)) e1 )
              with
              | SomeE tp0, SomeE tp1 ->
                  let iso =
                    is_iso_pure_expr_proof tp0 && is_iso_pure_expr_proof tp1
                  in
                    SomeE
                      (TPurePair
                         {
                           t0 = type_of_pure_expr_proof tp0;
                           t1 = type_of_pure_expr_proof tp1;
                           g;
                           d = d';
                           d0;
                           d1;
                           e0 = tp0;
                           e1 = tp1;
                           iso;
                         })
              | NoneE err, _
              | _, NoneE err ->
                  NoneE (err ^ error_addition)
      end
    (* T-CTRL *)
    | Ctrl (e', t0, t1, l) -> begin
        let l = List.sort (fun (e0, _) (e1, _) -> expr_compare e0 e1) l in
        let fve' = free_vars e' in
        let d0, d' = map_partition d fve' in
        let ejs, ej's = List.split l in
          begin
            match mixed_type_check g d0 e' with
            | SomeE tpe' when type_of_mixed_expr_proof tpe' = t0 -> begin
                match ortho_check t0 ejs false with
                | None -> NoneE ("Ortho check failed" ^ error_addition)
                | Some orp -> begin
                    match
                      map_all_or_nothing
                        (StringMap.mapi (fun x _ -> erases_check x ej's t1) d0)
                    with
                    | None -> NoneE ("Erasure check failed" ^ error_addition)
                    | Some erp -> begin
                        let pattern_result =
                          List.map (pure_pattern_type_check g d t0 t1) l
                        in
                          match
                            all_or_nothing
                              (List.map option_of_optionE pattern_result)
                          with
                          | Some l' ->
                              let iso =
                                expr_is_classical e'
                                && is_iso_mixed_expr_proof tpe'
                                && is_spanning_ortho_proof orp
                                && List.for_all
                                     (fun (_, _, tpej') ->
                                       is_iso_pure_expr_proof tpej')
                                     l'
                              in
                                SomeE
                                  (TCtrl
                                     {
                                       t0;
                                       t1;
                                       g;
                                       d = d0;
                                       d';
                                       e = tpe';
                                       l = l';
                                       orp;
                                       erp;
                                       iso;
                                     })
                          | _ -> begin
                              match
                                List.find
                                  (fun x -> option_of_optionE x = None)
                                  pattern_result
                              with
                              | NoneE err -> NoneE (err ^ error_addition)
                              | _ -> failwith "unreachable"
                            end
                      end
                  end
              end
            | NoneE err -> NoneE (err ^ error_addition)
            | _ -> NoneE ("Type mismatch" ^ error_addition)
          end
      end
    | Try _ ->
        NoneE ("Attempted to type Try as a pure expression" ^ error_addition)
    | Match _ ->
        NoneE ("Attempted to type Match as a pure expression" ^ error_addition)
    (* T-PUREAPP *)
    | Apply (f, e') -> begin
        match (pure_type_check g d e', prog_type_check f) with
        | SomeE tpe', SomeE tpf -> begin
            match (tpf, type_of_prog_proof tpf) with
            | MixedProg _, _ ->
                NoneE
                  ("Attempted pure application of mixed program"
                 ^ error_addition)
            | PureProg tpf, (t0, t1) -> begin
                let t' = type_of_pure_expr_proof tpe' in
                  if t' = t0 then
                    let iso =
                      is_iso_pure_prog_proof tpf && is_iso_pure_expr_proof tpe'
                    in
                      SomeE
                        (TPureApp
                           { t = t0; t' = t1; g; d; f = tpf; e = tpe'; iso })
                  else
                    NoneE ("Type mismatch in Apply" ^ error_addition)
              end
          end
        | NoneE err, _
        | _, NoneE err ->
            NoneE (err ^ error_addition)
      end

(** Given a mixed expression [e] expected to be of type [t] with classical
    context [g], finds the quantum context of the expression. *)
and mixed_context_check (g : context) (t : exprtype) (e : expr) :
    context optionE =
  let error_addition =
    Printf.sprintf
      "\n\n\
       in %s\n\
       when computing quantum context, typed as a mixed expression of type %s \
       with g = %s"
      (string_of_expr e) (string_of_type t) (string_of_context g)
  in
    match (e, t) with
    (* T-MIXEDPAIR *)
    | Qpair (e0, e1), ProdType (t0, t1) -> begin
        match (mixed_context_check g t0 e0, mixed_context_check g t1 e1) with
        | SomeE d0, SomeE d1 -> map_merge true d0 d1
        | NoneE err, _
        | _, NoneE err ->
            NoneE (err ^ error_addition)
      end
    | Qpair _, _ -> NoneE ("Expected product type" ^ error_addition)
    (* T-TRY *)
    | Try (e0, e1), _ -> begin
        match (mixed_context_check g t e0, mixed_context_check g t e1) with
        | SomeE d0, SomeE d1 -> map_merge false d0 d1
        | NoneE err, _
        | _, NoneE err ->
            NoneE (err ^ error_addition)
      end
    (* T-MATCH *)
    | Match (e', t0, t1, l), _ -> begin
        match
          (mixed_context_check g t0 e', match_dd1_context_check g t0 t1 l)
        with
        | NoneE err, _
        | _, NoneE err ->
            NoneE (err ^ error_addition)
        | SomeE dd0, SomeE dd1 -> SomeE (map_merge_noopt true dd0 dd1)
      end
    (* T-MIXEDAPP *)
    | Apply (f, e'), _ -> begin
        match prog_type_check f with
        | SomeE tpf -> begin
            let t0, t1 = type_of_prog_proof tpf in
              if t = t1 then
                mixed_context_check g t0 e'
              else
                NoneE ("Type mismatch" ^ error_addition)
          end
        | NoneE err -> NoneE (err ^ error_addition)
      end
    | _, _ -> pure_context_check g t e

(** Given a pure expression [e] expected to be of type [t] with classical
    context [g], finds the quantum context of the expression. *)
and pure_context_check (g : context) (t : exprtype) (e : expr) :
    context optionE =
  let error_addition =
    Printf.sprintf
      "\n\n\
       in %s\n\
       when computing quantum context, typed as a pure expression of type %s \
       with g = %s"
      (string_of_expr e) (string_of_type t) (string_of_context g)
  in
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
              NoneE
                (Printf.sprintf
                   "Type mismatch in classical context: expected %s, but got %s\n\
                    in %s"
                   (string_of_type t) (string_of_type t') (string_of_expr e)
                ^ error_addition)
        (* T-QVAR *)
        | None -> SomeE (StringMap.singleton x t)
      end
    (* T-PUREPAIR *)
    | Qpair (e0, e1), ProdType (t0, t1) -> begin
        match (pure_context_check g t0 e0, pure_context_check g t1 e1) with
        | SomeE d0, SomeE d1 -> map_merge true d0 d1
        | NoneE err, _
        | _, NoneE err ->
            NoneE (err ^ error_addition)
      end
    (* T-CTRL *)
    | Ctrl (e', t0, t1, l), _ -> begin
        match (mixed_context_check g t0 e', l) with
        | NoneE err, _ -> NoneE (err ^ error_addition)
        | SomeE d, [] -> SomeE d
        | SomeE d, (e0, e0') :: _ -> begin
            match first_pattern_context_check g t0 t1 e0 e0' with
            | NoneE err -> NoneE (err ^ error_addition)
            | SomeE d0 -> begin
                (* d is Γ, Δ g is Γ, Γ' d0 is Δ, Δ' *)
                match map_merge false g d0 with
                | NoneE err -> NoneE (err ^ error_addition)
                | SomeE gd0 ->
                    let ej, ej' = List.split l in
                      if not (map_is_inclusion d gd0) then
                        NoneE ("Context inclusion failed" ^ error_addition)
                      else if ortho_check t0 ej false = None then
                        NoneE ("Ortho check failed" ^ error_addition)
                      else if
                        not
                          (List.for_all
                             (fun x ->
                               option_of_optionE
                                 (pure_pattern_type_check g d0 t0 t1 x)
                               <> None)
                             l)
                      then
                        NoneE ("Type mismatch" ^ error_addition)
                      else if
                        not
                          (StringMap.for_all
                             (fun x _ -> erases_check x ej' t1 <> None)
                             d)
                      then
                        NoneE ("Erasure check failed" ^ error_addition)
                      else
                        SomeE d0
              end
          end
      end
    | Try _, _ ->
        NoneE ("Attempted to type Try as a pure expression" ^ error_addition)
    (* T-PUREAPP *)
    | Apply (f, e'), _ -> begin
        match prog_type_check f with
        | SomeE (PureProg tpf) -> begin
            let t0, t1 = type_of_prog_proof (PureProg tpf) in
              if t = t1 then
                pure_context_check g t0 e'
              else
                NoneE ("Type mismatch" ^ error_addition)
          end
        | SomeE (MixedProg _) ->
            NoneE
              ("Attempted pure application of mixed program" ^ error_addition)
        | NoneE err -> NoneE (err ^ error_addition)
      end
    | _, _ -> NoneE ("Type mismatch" ^ error_addition)

(** Typechecks a program [f]. *)
and prog_type_check (f : prog) : prog_typing_proof optionE =
  let error_addition = Printf.sprintf "\n\nin %s" (string_of_prog f) in
    match f with
    (* T-GATE *)
    | U3 (theta, phi, lambda) -> SomeE (PureProg (TGate (theta, phi, lambda)))
    (* T-LEFT *)
    | Left (t0, t1) -> SomeE (PureProg (TLeft (t0, t1)))
    (* T-RIGHT *)
    | Right (t0, t1) -> SomeE (PureProg (TRight (t0, t1)))
    | Lambda (e, t, e') -> begin
        match pure_context_check StringMap.empty t e with
        | NoneE err -> NoneE (err ^ error_addition)
        | SomeE d -> begin
            match
              ( pure_type_check StringMap.empty d e,
                pure_type_check StringMap.empty d e' )
            with
            (* T-PUREABS *)
            | SomeE tpe, SomeE tpe' ->
                let iso =
                  is_un_pure_expr_proof tpe && is_iso_pure_expr_proof tpe'
                in
                  SomeE
                    (PureProg
                       (TPureAbs
                          {
                            t;
                            t' = type_of_pure_expr_proof tpe';
                            d;
                            e = tpe;
                            e' = tpe';
                            iso;
                          }))
            | _ -> begin
                match
                  ( pure_type_check StringMap.empty d e,
                    mixed_type_check StringMap.empty d e' )
                with
                | NoneE err, _
                | _, NoneE err ->
                    NoneE (err ^ error_addition)
                (* T-MIXEDABS *)
                | SomeE tpe, SomeE tpe' ->
                    let iso =
                      is_iso_pure_expr_proof tpe
                      && is_iso_mixed_expr_proof tpe'
                    in
                      SomeE
                        (MixedProg
                           (TMixedAbs
                              {
                                t;
                                t' = type_of_mixed_expr_proof tpe';
                                d;
                                e = tpe;
                                e' = tpe';
                                iso;
                              }))
              end
          end
      end
    (* T-RPHASE *)
    | Rphase (t, e', r0, r1) -> begin
        match pure_context_check StringMap.empty t e' with
        | NoneE err -> NoneE (err ^ error_addition)
        | SomeE d -> begin
            match pure_type_check StringMap.empty d e' with
            | NoneE err -> NoneE (err ^ error_addition)
            | SomeE tpe' when type_of_pure_expr_proof tpe' = t ->
                let iso = is_iso_pure_expr_proof tpe' in
                  SomeE (PureProg (TRphase { t; e = tpe'; r0; r1; iso }))
            | _ -> NoneE ("Type mismatch" ^ error_addition)
          end
      end
    (* T-PMATCH *)
    | Pmatch (t0, t1, l) -> begin
        let ejs = List.map fst l in
        let ej's = List.map snd l in
        let perm0 =
          List.map snd
            (List.sort
               (fun (e0, _) (e1, _) -> expr_compare e0 e1)
               (List.combine ejs (range (List.length l))))
        in
        let perm1 =
          List.map snd
            (List.sort
               (fun (e0, _) (e1, _) -> expr_compare e0 e1)
               (List.combine ej's (range (List.length l))))
        in
          match
            all_or_nothing_optionE
              (List.map
                 (fun (ej, ej') ->
                   match
                     ( pure_context_check StringMap.empty t0 ej,
                       pure_context_check StringMap.empty t1 ej' )
                   with
                   | SomeE d0, SomeE d1 ->
                       if StringMap.equal ( = ) d0 d1 then begin
                         match
                           ( pure_type_check StringMap.empty d0 ej,
                             pure_type_check StringMap.empty d0 ej' )
                         with
                         | SomeE tp0, SomeE tp1 -> SomeE (d0, tp0, tp1)
                         | NoneE err, _
                         | _, NoneE err ->
                             NoneE err
                       end
                       else
                         NoneE ("Context mismatch" ^ error_addition)
                   | NoneE err, _
                   | _, NoneE err ->
                       NoneE err)
                 l)
          with
          | NoneE err -> NoneE err
          | SomeE l' -> begin
              match
                ( ortho_check t0 (List.map fst l) true,
                  ortho_check t1 (List.map snd l) true )
              with
              | Some orp0, Some orp1 -> begin
                  let iso = is_spanning_ortho_proof orp0 in
                    SomeE
                      (PureProg
                         (TPmatch
                            { t0; t1; l = l'; orp0; orp1; perm0; perm1; iso }))
                end
              | _ -> NoneE ("Ortho check failed" ^ error_addition)
            end
      end

(** Typechecks a pure expression, throwing an exception in the case of a
    typechecking error. *)
let pure_type_check_noopt (e : expr) : pure_expr_typing_proof =
  match pure_type_check StringMap.empty StringMap.empty e with
  | SomeE t -> t
  | NoneE err -> failwith err

(** Typechecks a mixed expression, throwing an exception in the case of a
    typechecking error. *)
let mixed_type_check_noopt (e : expr) : mixed_expr_typing_proof =
  match mixed_type_check StringMap.empty StringMap.empty e with
  | SomeE t -> t
  | NoneE err -> failwith err

(** Typechecks a program, throwing an exception in the case of a typechecking
    error. *)
let prog_type_check_noopt (f : prog) : prog_typing_proof =
  match prog_type_check f with
  | SomeE ft -> ft
  | NoneE err -> failwith err
