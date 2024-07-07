open Util
open Syntax

type context = exprtype StringMap.t

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

let rec erases_check (x : string) (l : expr list) : bool =
  let l' = List.flatten (List.map dephase l) in
    if List.for_all (fun e' -> e' = Var x) l' then
      true
    else
      match split_qpair_list l' with
      | Some (l0, l1) -> erases_check x l0 || erases_check x l1
      | None -> false

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

let missing_span (t : exprtype) (l : expr list) : expr list option =
  let rec missing_span_helper (t : exprtype) (l : expr list) (fv : StringSet.t)
      : expr list option =
    match (t, l) with
    | Qunit, [] -> Some [Null]
    | Qunit, [Null] -> Some []
    | _, [] -> Some [Var (fresh_string "_" fv)]
    | _, [Var x] -> if StringSet.mem x fv then None else Some []
    | SumType (t0, t1), _ -> begin
        match split_sum_list t0 t1 l with
        | None -> None
        | Some (l0, l1) -> begin
            match
              (missing_span_helper t0 l0 fv, missing_span_helper t1 l1 fv)
            with
            | Some l0', Some l1' ->
                Some
                  (List.map (fun x -> Apply (Left (t0, t1), x)) l0'
                  @ List.map (fun x -> Apply (Right (t0, t1), x)) l1')
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
              | Some l0 -> begin
                  let l'' = List.map (fun e0 -> (e0, [])) l0 @ l' in
                  let result =
                    all_or_nothing
                      (List.map
                         (fun (e0, l1) ->
                           match
                             missing_span_helper t1 l1
                               (StringSet.union fv (free_vars e0))
                           with
                           | Some l1' ->
                               Some (List.map (fun e1 -> Qpair (e0, e1)) l1')
                           | None -> None)
                         l'')
                  in
                    match result with
                    | Some r -> Some (List.flatten r)
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
  | None -> None
  | Some l' -> Some (l @ l')

let ortho_check (t : exprtype) (l : expr list) : bool =
  match span_list t l with
  | Some _ -> true
  | None -> false

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
    (t' : exprtype) ((ej, ej') : expr * expr) : bool =
  match context_check StringMap.empty t ej with
  | NoneE _ -> false
  | SomeE gj -> begin
      match map_merge false g gj with
      | SomeE ggj -> pure_type_check ggj d ej' = SomeE t'
      | NoneE _ -> false
    end

(*
Finds the type of a mixed expression e with a quantum context d
*)
and mixed_type_check (d : context) (e : expr) : exprtype optionE =
  match e with
  (* T-MIXEDPAIR *)
  | Qpair (e0, e1) -> begin
      if
        StringSet.diff (map_dom d)
          (StringSet.union (free_vars e0) (free_vars e1))
        <> StringSet.empty
      then
        NoneE "Irrelevant variables in quantum context in mixed Qpair"
      else
        match
          ( mixed_type_check (map_restriction d (free_vars e0)) e0,
            mixed_type_check (map_restriction d (free_vars e1)) e1 )
        with
        | SomeE t0, SomeE t1 -> SomeE (ProdType (t0, t1))
        | NoneE err, _
        | _, NoneE err ->
            NoneE (err ^ "\nin mixed Qpair")
    end
  (* T-TRY *)
  | Try (e0, e1) -> begin
      let d0, d1 = map_partition d (free_vars e0) in
        match (mixed_type_check d0 e0, mixed_type_check d1 e1) with
        | SomeE t0, SomeE t1 ->
            if t0 = t1 then SomeE t0 else NoneE "Type mismatch in Try"
        | NoneE err, _
        | _, NoneE err ->
            NoneE (err ^ "\nin Try")
    end
  (* T-MIXEDAPP *)
  | Apply (f, e') -> begin
      match (mixed_type_check d e', prog_type_check f) with
      | SomeE t', SomeE (Coherent (t0, t1))
      | SomeE t', SomeE (Channel (t0, t1)) ->
          if t' = t0 then
            SomeE t1
          else
            NoneE "Type mismatch in mixed Apply"
      | NoneE err, _
      | _, NoneE err ->
          NoneE (err ^ "\nin mixed Apply")
    end
  | _ -> pure_type_check StringMap.empty d e

(*
Finds the type of a pure expression e with a classical context g and a
quantum context d
*)
and pure_type_check (g : context) (d : context) (e : expr) : exprtype optionE =
  match e with
  (* T-UNIT *)
  | Null -> begin
      match StringMap.bindings d with
      | [] -> SomeE Qunit
      | _ -> NoneE "Irrelevant variables in quantum context in Null"
    end
  | Var x -> begin
      match StringMap.bindings d with
      | [] -> begin
          (* T-CVAR *)
          match StringMap.find_opt x g with
          | Some t -> SomeE t
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
            | None -> SomeE t
          end
      | _ -> NoneE "Irrelevant variables in quantum context in Var"
    end
  (* T-PUREPAIR *)
  | Qpair (e0, e1) -> begin
      if
        StringSet.diff (map_dom d)
          (StringSet.union (free_vars e0) (free_vars e1))
        <> StringSet.empty
      then
        NoneE "Irrelevant variables in quantum context in Qpair"
      else
        match
          ( pure_type_check g (map_restriction d (free_vars e0)) e0,
            pure_type_check g (map_restriction d (free_vars e1)) e1 )
        with
        | SomeE t0, SomeE t1 -> SomeE (ProdType (t0, t1))
        | NoneE err, _
        | _, NoneE err ->
            NoneE (err ^ "\nin Qpair")
    end
  (* T-CTRL *)
  | Ctrl (e', t0, l, t1) -> begin
      let fve' = free_vars e' in
      let g0 = map_restriction g fve' in
      let d0 = map_restriction d fve' in
      let ej, ej' = List.split l in
        match map_merge false g0 d0 with
        | NoneE err -> NoneE (err ^ "\nin Ctrl")
        | SomeE g0d0 ->
            if mixed_type_check g0d0 e' <> SomeE t0 then
              NoneE "Type mismatch in Ctrl"
            else if not (ortho_check t0 ej) then
              NoneE "Ortho check failed in Ctrl"
            else if not (StringMap.for_all (fun x _ -> erases_check x ej') d0)
            then
              NoneE "Erasure check failed in Ctrl"
            else if not (List.for_all (pattern_type_check g d t0 t1) l) then
              NoneE "Type mismatch in Ctrl"
            else
              SomeE t1
    end
  | Try _ -> NoneE "Try is not a pure expression"
  (* T-PUREAPP *)
  | Apply (f, e') -> begin
      match (pure_type_check g d e', prog_type_check f) with
      | SomeE t', SomeE (Coherent (t0, t1)) ->
          if t' = t0 then
            SomeE t1
          else
            NoneE "Type mismatch in Apply"
      | SomeE _, SomeE (Channel _) ->
          NoneE "Attempted pure application of mixed program"
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
      | SomeE (Coherent (t0, t1))
      | SomeE (Channel (t0, t1)) ->
          if t = t1 then
            mixed_context_check t0 e'
          else
            NoneE "Type mismatch in mixed Apply"
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
      | _, [] -> NoneE "Control block cannot be empty"
      | SomeE d, (e0, e0') :: _ -> begin
          match first_pattern_context_check g t0 t1 e0 e0' with
          | NoneE err -> NoneE (err ^ "\nin Ctrl")
          | SomeE d0 -> begin
              (* d is Gamma, Delta
                 g is Gamma, Gamma'
                 d0 is Delta, Delta' *)
              match map_merge false g d0 with
              | NoneE err -> NoneE (err ^ "\nin Ctrl")
              | SomeE gd0 ->
                  let ej, ej' = List.split l in
                    if not (map_inclusion d gd0) then
                      NoneE "Context inclusion failed in Ctrl"
                    else if not (ortho_check t0 ej) then
                      NoneE "Ortho check failed in Ctrl"
                    else if
                      not (List.for_all (pattern_type_check g d0 t0 t1) l)
                    then
                      NoneE "Type mismatch in Ctrl"
                    else if
                      not (StringMap.for_all (fun x _ -> erases_check x ej') d)
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
      | SomeE (Coherent (t0, t1)) ->
          if t = t1 then
            context_check g t0 e'
          else
            NoneE "Type mismatch in pure Apply"
      | SomeE (Channel _) ->
          NoneE "Attempted pure application of mixed program"
      | NoneE err -> NoneE (err ^ "\nin Apply")
    end
  | _, _ -> NoneE "Type mismatch"

and prog_type_check (f : prog) : progtype optionE =
  match f with
  (* T-GATE *)
  | U3 _ -> SomeE (Coherent (bit, bit))
  (* T-LEFT *)
  | Left (t0, t1) -> SomeE (Coherent (t0, SumType (t0, t1)))
  (* T-RIGHT *)
  | Right (t0, t1) -> SomeE (Coherent (t1, SumType (t0, t1)))
  | Lambda (e, t, e') -> begin
      match context_check StringMap.empty t e with
      | NoneE err -> NoneE (err ^ "\nin Lambda")
      | SomeE d -> begin
          match pure_type_check StringMap.empty d e' with
          (* T-PUREABS *)
          | SomeE t' -> SomeE (Coherent (t, t'))
          | NoneE _ -> begin
              match mixed_type_check (map_restriction d (free_vars e')) e' with
              | NoneE err -> NoneE (err ^ "\nin Lambda")
              (* T-MIXEDABS *)
              | SomeE t' -> SomeE (Channel (t, t'))
            end
        end
    end
  (* T-RPHASE *)
  | Rphase (t, er, _, _) -> begin
    match context_check StringMap.empty t er with
    | NoneE err -> NoneE (err ^ "\nin Rphase")
    | SomeE d -> begin
      match pure_type_check StringMap.empty d er with
      | NoneE err -> NoneE (err ^ "\nin Rphase")
      | SomeE t' when t' = t -> SomeE (Coherent (t, t))
      | _ -> NoneE "Type mismatch in Rphase"
    end
  end

let expr_typecheck_noopt (e : expr) =
  match mixed_type_check StringMap.empty e with
  | SomeE t -> t
  | NoneE err -> failwith err

let prog_typecheck_noopt (f : prog) =
  match prog_type_check f with
  | SomeE ft -> ft
  | NoneE err -> failwith err
