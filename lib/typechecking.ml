open Syntax

type 'a optionE = SomeE of 'a | NoneE of string

module Context = Map.Make (String)

type context = exprtype Context.t

module StringSet = Set.Make (String)

let option_of_optionE (optE : 'a optionE) : 'a option =
  match optE with
  | NoneE _ -> None
  | SomeE x -> Some x

(*
Combine two contexts into one. If allow_dup is false: fails if
the contexts share any bindings. If allow_dup is false: fails only if
there is a type mismatch.
*)
let context_merge (allow_dup : bool) (d0 : context) (d1 : context) :
    context optionE =
  let merged_with_opt =
    Context.merge
      (fun _ a b ->
        match (a, b) with
        | None, None -> None
        | Some a', None -> Some (Some a')
        | None, Some b' -> Some (Some b')
        | Some a', Some b' ->
            if allow_dup && a' = b' then Some (Some a') else Some None)
      d0 d1
  in
    if Context.for_all (fun _ x -> x <> None) merged_with_opt then
      SomeE
        (Context.map
           (fun x ->
             match x with
             | Some x' -> x'
             | None -> failwith "unreachable")
           merged_with_opt)
    else
      NoneE
        (if allow_dup then
           "Type mismatch in context merge"
         else
           "Expected disjoint contexts")

(* Whether a context is included in another context *)
let context_inclusion (d0 : context) (d1 : context) : bool =
  Context.for_all (fun x t -> Context.find_opt x d1 = Some t) d0

let context_dom (d : context) : StringSet.t =
  StringSet.of_list (List.map fst (Context.bindings d))

(* Restrict context to include only variables from a given set *)
let context_restriction (d : context) (s : StringSet.t) : context =
  Context.filter (fun x _ -> StringSet.mem x s) d

(* Partition context d into part belonging to s and part not belonging to s *)
let context_partition (d : context) (s : StringSet.t) : context * context =
  ( context_restriction d s,
    Context.filter (fun x _ -> not (StringSet.mem x s)) d )

let fresh_string (s : StringSet.t) : string =
  let rec fresh_string_helper (i : int) =
    let candidate = "$" ^ string_of_int i in
      if StringSet.mem candidate s then
        fresh_string_helper (i + 1)
      else
        candidate
  in
    fresh_string_helper 0

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
  | Apply (Gphase _, e') -> dephase e'
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

(*
Given an expression type t and list l of expressions, expected to be of
type t, extends the list to one "spanning" t.
*)
let span_list (t : exprtype) (l : expr list) : expr list option =
  let rec missing_span (t : exprtype) (l : expr list) (fv : StringSet.t) :
      expr list option =
    match (t, l) with
    | Qunit, [] -> Some [Null]
    | Qunit, [Null] -> Some []
    | _, [] -> Some [Var (fresh_string fv)]
    | _, [Var x] -> if StringSet.mem x fv then None else Some []
    | SumType (t0, t1), _ -> begin
        match split_sum_list t0 t1 l with
        | None -> None
        | Some (l0, l1) -> begin
            match (missing_span t0 l0 fv, missing_span t1 l1 fv) with
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
              match missing_span t0 (List.map fst l') next_fv with
              | None -> None
              | Some l0 ->
                  let l'' = List.map (fun e0 -> (e0, [])) l0 @ l' in
                  let result_with_opt =
                    List.map
                      (fun (e0, l1) ->
                        match
                          missing_span t1 l1
                            (StringSet.union fv (free_vars e0))
                        with
                        | Some l1' ->
                            Some (List.map (fun e1 -> Qpair (e0, e1)) l1')
                        | None -> None)
                      l''
                  in
                    if List.for_all (fun l1 -> l1 <> None) result_with_opt then
                      Some
                        (List.flatten
                           (List.map
                              (fun l1 ->
                                match l1 with
                                | Some l1' -> l1'
                                | None -> failwith "unreachable")
                              result_with_opt))
                    else
                      None
          end
      end
    | _, _ -> None
  in
    match missing_span t l StringSet.empty with
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
  match context_check Context.empty t ej with
  | NoneE err -> NoneE err
  | SomeE gj -> begin
      match context_merge false g gj with
      | SomeE ggj -> context_check ggj t' ej'
      | NoneE err -> NoneE err
    end

(*
In T-CTRL, checks the type of an expression on the right-hand side of a
control block under the contexts obtained from the first pattern in the block.
*)
and pattern_type_check (g : context) (d : context) (t : exprtype)
    (t' : exprtype) ((ej, ej') : expr * expr) : bool =
  match context_check Context.empty t ej with
  | NoneE _ -> false
  | SomeE gj -> begin
      match context_merge false g gj with
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
        StringSet.diff (context_dom d)
          (StringSet.union (free_vars e0) (free_vars e1))
        <> StringSet.empty
      then
        NoneE "Irrelevant variables in quantum context"
      else
        match
          ( mixed_type_check (context_restriction d (free_vars e0)) e0,
            mixed_type_check (context_restriction d (free_vars e1)) e1 )
        with
        | SomeE t0, SomeE t1 -> SomeE (ProdType (t0, t1))
        | NoneE err, _
        | _, NoneE err ->
            NoneE err
    end
  (* T-TRY *)
  | Try (e0, e1) -> begin
      let d0, d1 = context_partition d (free_vars e0) in
        match (mixed_type_check d0 e0, mixed_type_check d1 e1) with
        | SomeE t0, SomeE t1 ->
            if t0 = t1 then SomeE t0 else NoneE "Type mismatch in try block"
        | NoneE err, _
        | _, NoneE err ->
            NoneE err
    end
  (* T-MIXEDAPP *)
  | Apply (f, e') -> begin
      match (mixed_type_check d e', prog_type_check f) with
      | SomeE t', SomeE (Coherent (t0, t1))
      | SomeE t', SomeE (Channel (t0, t1)) ->
          if t' = t0 then
            SomeE t1
          else
            NoneE "Type mismatch in mixed application"
      | NoneE err, _
      | _, NoneE err ->
          NoneE err
    end
  | _ -> pure_type_check Context.empty d e

(*
Finds the type of a pure expression e with a classical context g and a
quantum context d
*)
and pure_type_check (g : context) (d : context) (e : expr) : exprtype optionE =
  match e with
  (* T-UNIT *)
  | Null -> SomeE Qunit
  | Var x -> begin
      match Context.bindings d with
      | [] -> begin
          (* T-CVAR *)
          match Context.find_opt x g with
          | Some t -> SomeE t
          | None -> NoneE "Unbound variable"
        end
      | [(x', t)] ->
          if x' <> x then
            NoneE "Irrelevant variables in quantum context"
          else begin (* T-QVAR *)
            match Context.find_opt x g with
            | Some _ ->
                NoneE "Variable appears in both classical and quantum context"
            | None -> SomeE t
          end
      | _ -> NoneE "Irrelevant variables in quantum context"
    end
  (* T-PUREPAIR *)
  | Qpair (e0, e1) -> begin
      if
        StringSet.diff (context_dom d)
          (StringSet.union (free_vars e0) (free_vars e1))
        <> StringSet.empty
      then
        NoneE "Irrelevant variables in quantum context"
      else
        match
          ( pure_type_check g (context_restriction d (free_vars e0)) e0,
            pure_type_check g (context_restriction d (free_vars e1)) e1 )
        with
        | SomeE t0, SomeE t1 -> SomeE (ProdType (t0, t1))
        | NoneE err, _
        | _, NoneE err ->
            NoneE err
    end
  (* T-CTRL *)
  | Ctrl (e', t0, l, t1) -> begin
      let fve' = free_vars e' in
      let g0 = context_restriction g fve' in
      let d0 = context_restriction d fve' in
      let ej, ej' = List.split l in
        match context_merge false g0 d0 with
        | NoneE err -> NoneE err
        | SomeE g0d0 ->
            if mixed_type_check g0d0 e' <> SomeE t0 then
              NoneE "Type mismatch in control expression"
            else if not (ortho_check t0 ej) then
              NoneE "Ortho check failed"
            else if not (List.for_all (pattern_type_check g d t0 t1) l) then
              NoneE "Type mismatch in control block"
            else if not (Context.for_all (fun x _ -> erases_check x ej') d0)
            then
              NoneE "Erasure check failed"
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
            NoneE "Type mismatch in pure application"
      | SomeE _, SomeE (Channel _) ->
          NoneE "Attempted pure application of mixed program"
      | NoneE err, _
      | _, NoneE err ->
          NoneE err
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
      | SomeE d0, SomeE d1 -> context_merge true d0 d1
      | NoneE err, _
      | _, NoneE err ->
          NoneE err
    end
  | Qpair _, _ -> NoneE "Expected product type for mixed pair"
  (* T-TRY *)
  | Try (e0, e1), _ -> begin
      match (mixed_context_check t e0, mixed_context_check t e1) with
      | SomeE d0, SomeE d1 -> context_merge false d0 d1
      | NoneE err, _
      | _, NoneE err ->
          NoneE err
    end
  (* T-MIXEDAPP *)
  | Apply (f, e'), _ -> begin
      match prog_type_check f with
      | SomeE (Coherent (t0, t1))
      | SomeE (Channel (t0, t1)) ->
          if t = t1 then
            mixed_context_check t0 e'
          else
            NoneE "Type mismatch in mixed application"
      | NoneE err -> NoneE err
    end
  | _, _ -> context_check Context.empty t e

(*
Given a pure expression e expected to be of type t with classical context g,
find the quantum context of the expression.
*)
and context_check (g : context) (t : exprtype) (e : expr) : context optionE =
  match (e, t) with
  (* T-UNIT *)
  | Null, Qunit -> SomeE Context.empty
  | Var x, _ -> begin
      match Context.find_opt x g with
      (* T-CVAR *)
      | Some t' ->
          if t = t' then
            SomeE Context.empty
          else
            NoneE "Type mismatch in classical context"
      (* T-QVAR *)
      | None -> SomeE (Context.singleton x t)
    end
  (* T-PUREPAIR *)
  | Qpair (e0, e1), ProdType (t0, t1) -> begin
      match (context_check g t0 e0, context_check g t1 e1) with
      | SomeE d0, SomeE d1 -> context_merge true d0 d1
      | NoneE err, _
      | _, NoneE err ->
          NoneE err
    end
  (* T-CTRL *)
  | Ctrl (e', t0, l, t1), _ -> begin
      match (mixed_context_check t0 e', l) with
      | NoneE err, _ -> NoneE err
      | _, [] -> NoneE "Control block cannot be empty"
      | SomeE d, (e0, e0') :: _ -> begin
          match first_pattern_context_check g t0 t1 e0 e0' with
          | NoneE err -> NoneE err
          | SomeE d0 -> begin
              (* d is Gamma, Delta
                 g is Gamma, Gamma'
                 d0 is Delta, Delta' *)
              match context_merge false g d0 with
              | NoneE err -> NoneE err
              | SomeE gd0 ->
                  let ej, ej' = List.split l in
                    if not (context_inclusion d gd0) then
                      NoneE "Context inclusion failed"
                    else if not (ortho_check t0 ej) then
                      NoneE "Ortho check failed"
                    else if
                      not (List.for_all (pattern_type_check g d0 t0 t1) l)
                    then
                      NoneE "Type mismatch in control block"
                    else if
                      not (Context.for_all (fun x _ -> erases_check x ej') d)
                    then
                      NoneE "Erasure check failed"
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
            NoneE "Type mismatch in pure application"
      | SomeE (Channel _) ->
          NoneE "Attempted pure application of mixed program"
      | NoneE err -> NoneE err
    end
  | _, _ -> NoneE "Type mismatch"

and prog_type_check (p : prog) : progtype optionE =
  match p with
  (* T-GATE *)
  | U3 _ -> SomeE (Coherent (bit, bit))
  (* T-LEFT *)
  | Left (t, t') -> SomeE (Coherent (t, SumType (t, t')))
  (* T-RIGHT *)
  | Right (t, t') -> SomeE (Coherent (t', SumType (t, t')))
  | Lambda (e, t, e') -> begin
      match context_check Context.empty t e with
      | NoneE err -> NoneE err
      | SomeE d -> (
          match pure_type_check Context.empty d e' with
          (* T-PUREABS *)
          | SomeE t' -> SomeE (Coherent (t, t'))
          | NoneE _ -> (
              match
                mixed_type_check (context_restriction d (free_vars e')) e'
              with
              | NoneE err -> NoneE err
              (* T-MIXEDABS *)
              | SomeE t' -> SomeE (Channel (t, t'))))
    end
  (* T-GPHASE *)
  | Gphase (t, _) -> SomeE (Coherent (t, t))
