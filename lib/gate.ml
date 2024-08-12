open Util
open Reals
open Matrix

(*
Low-level representation of a quantum circuit. Describes unitary gates (or
reset operations) acting on qubits specified by integer labels.
*)
type gate =
  | Identity
  | U3Gate of (int * real * real * real)
  | GphaseGate of real
  | Reset of int
  | MeasureAsErr of int
  | PotentialDeletionLabel of int
  | Swap of int * int
  | Annotation of int list * string
  | Controlled of (int list * bool list * gate)
  | Sequence of (gate * gate)

let ( @& ) a b = Sequence (a, b)

(* Simple utility gate definitions *)
let gate_paulix (i : int) : gate = U3Gate (i, Pi, Const 0, Pi)
let gate_pauliz (i : int) : gate = U3Gate (i, Const 0, Pi, Const 0)
let gate_had (i : int) : gate = U3Gate (i, Div (Pi, Const 2), Const 0, Pi)

let gate_cnot (b0 : int) (b1 : int) : gate =
  Controlled ([b0], [true], gate_paulix b1)

let gate_is_unitary (u : gate) : bool =
  match u with
  | Reset _ -> false
  | MeasureAsErr _ -> false
  | _ -> true

(*
Take the adjoint of a unitary gate.
*)
let rec gate_adjoint (u : gate) : gate =
  match u with
  | Identity -> Identity
  | U3Gate (i, theta, phi, lambda) ->
      if u = gate_paulix i || u = gate_pauliz i || u = gate_had i then
        u
      else
        U3Gate (i, Negate theta, Negate lambda, Negate phi)
  | GphaseGate theta -> GphaseGate (Negate theta)
  | Reset _ -> failwith "Can't take adjoint of reset"
  | MeasureAsErr _ -> failwith "Can't take adjoint of measurement"
  | PotentialDeletionLabel _ -> Identity
  | Swap (i, j) -> Swap (i, j)
  | Annotation (l, s) -> Annotation (l, s)
  | Controlled (l, bl, u0) -> Controlled (l, bl, gate_adjoint u0)
  | Sequence (u0, u1) -> Sequence (gate_adjoint u1, gate_adjoint u0)

(*
These are some sufficient (but not necessary) conditions for two gates to
be equal.
*)
let rec gate_equal (u0 : gate) (u1 : gate) =
  match (u0, u1) with
  | U3Gate (i0, theta0, phi0, lambda0), U3Gate (i1, theta1, phi1, lambda1) ->
      i0 = i1 && real_equal theta0 theta1 && real_equal phi0 phi1
      && real_equal lambda0 lambda1
  | GphaseGate theta0, GphaseGate theta1 -> real_equal theta0 theta1
  | Controlled (l0, bl0, u0), Controlled (l1, bl1, u1) ->
      let lbl0 = List.combine l0 bl0 in
      let lbl1 = List.combine l1 bl1 in
        List.sort (fun (i, _) (j, _) -> i - j) lbl0
        = List.sort (fun (i, _) (j, _) -> i - j) lbl1
        && bl0 = bl1 && gate_equal u0 u1
  | Sequence (u00, u01), Sequence (u10, u11) ->
      gate_equal u00 u10 && gate_equal u01 u11
  | Swap (i, j), Swap (i', j') -> (i = i' && j = j') || (i = j' && j = i')
  | _ -> u0 = u1

let gate_is_x (u : gate) : bool =
  match u with
  | U3Gate (i, _, _, _) -> gate_equal u (gate_paulix i)
  | _ -> false

let rec gate_qubits_used (u : gate) : IntSet.t =
  match u with
  | Identity -> IntSet.empty
  | U3Gate (i, _, _, _) -> IntSet.singleton i
  | GphaseGate _ -> IntSet.empty
  | Reset i
  | MeasureAsErr i
  | PotentialDeletionLabel i ->
      IntSet.singleton i
  | Swap (i, j) -> IntSet.of_list [i; j]
  | Annotation (l, _) -> IntSet.of_list l
  | Controlled (l, _, u0) ->
      IntSet.union (IntSet.of_list l) (gate_qubits_used u0)
  | Sequence (u0, u1) ->
      IntSet.union (gate_qubits_used u0) (gate_qubits_used u1)

let rec gate_num_err_measurements (u : gate) : int =
  match u with
  | MeasureAsErr _ -> 1
  | Sequence (u0, u1) ->
      gate_num_err_measurements u0 + gate_num_err_measurements u1
  | _ -> 0

(*
Rewire a gate by replacing all references to certain qubits with
other qubits.
*)
let rec gate_rewire (u : gate) (source : int list) (target : int list) : gate =
  let rewiring = IntMap.of_seq (List.to_seq (List.combine source target)) in
    match u with
    | Identity -> Identity
    | U3Gate (i, theta, phi, lambda) ->
        U3Gate (int_map_find_or_keep rewiring i, theta, phi, lambda)
    | GphaseGate theta -> GphaseGate theta
    | Reset i -> Reset (int_map_find_or_keep rewiring i)
    | MeasureAsErr i -> MeasureAsErr (int_map_find_or_keep rewiring i)
    | PotentialDeletionLabel i ->
        PotentialDeletionLabel (int_map_find_or_keep rewiring i)
    | Swap (i, j) ->
        Swap (int_map_find_or_keep rewiring i, int_map_find_or_keep rewiring j)
    | Annotation (l, s) ->
        Annotation (List.map (int_map_find_or_keep rewiring) l, s)
    | Controlled (l, bl, u0) ->
        Controlled
          ( List.map (int_map_find_or_keep rewiring) l,
            bl,
            gate_rewire u0 source target )
    | Sequence (u0, u1) ->
        Sequence (gate_rewire u0 source target, gate_rewire u1 source target)

(*
Remove all identity gates from a circuit, unless the entire circuit is
equivalent to an identity, in which case return the identity gate.
*)
let rec gate_remove_identities (u : gate) : gate =
  match u with
  | Controlled (l, bl, u0) -> begin
      let u0' = gate_remove_identities u0 in
        if u0' = Identity then Identity else Controlled (l, bl, u0')
    end
  | Sequence (Identity, Identity) -> Identity
  | Sequence (Identity, u0) -> gate_remove_identities u0
  | Sequence (u0, Identity) -> gate_remove_identities u0
  | Sequence (u0, u1) -> begin
      let u0' = gate_remove_identities u0 in
      let u1' = gate_remove_identities u1 in
        match (u0', u1') with
        | Identity, _ -> u1'
        | _, Identity -> u0'
        | _, _ -> Sequence (u0', u1')
    end
  | _ -> u

(*
Replace controlled sequences of gates with sequences of controlled gates,
making the only controlled gates in the end be single-qubit gates,
global phase gates, and swap gates.
*)
let rec gate_combine_and_distribute_controls (u : gate) : gate =
  match u with
  | Controlled (l, bl, u0) -> begin
      match gate_combine_and_distribute_controls u0 with
      | Controlled (l', bl', u0') ->
          gate_combine_and_distribute_controls
            (Controlled (l @ l', bl @ bl', u0'))
      | Sequence (u0, u1) ->
          gate_combine_and_distribute_controls
            (Sequence
               ( Controlled (l, bl, gate_combine_and_distribute_controls u0),
                 Controlled (l, bl, gate_combine_and_distribute_controls u1) ))
      | u0' -> Controlled (l, bl, u0')
    end
  | Sequence (u0, u1) ->
      Sequence
        ( gate_combine_and_distribute_controls u0,
          gate_combine_and_distribute_controls u1 )
  | _ -> u

let rec gate_to_list (u : gate) : gate list =
  match u with
  | Sequence (u0, u1) -> gate_to_list u0 @ gate_to_list u1
  | _ -> [u]

let rec gate_of_list (ul : gate list) : gate =
  match ul with
  | [] -> Identity
  | [u] -> u
  | u :: ul' -> Sequence (u, gate_of_list ul')

(*
Share a register to another one by applying CNOT gates for each qubit.
*)
let rec gate_share (reg0 : int list) (reg1 : int list) : gate =
  match (reg0, reg1) with
  | [], [] -> Identity
  | h1 :: t1, h2 :: t2 -> gate_cnot h1 h2 @& gate_share t1 t2
  | _ -> invalid_arg "Registers must be of the same size"

(*
Swap two registers of equal size.
*)
let rec gate_swap_regs (reg0 : int list) (reg1 : int list) : gate =
  match (reg0, reg1) with
  | [], [] -> Identity
  | h1 :: t1, h2 :: t2 -> Swap (h1, h2) @& gate_swap_regs t1 t2
  | _ -> invalid_arg "Registers must be of the same size"

(*
A gate that sends the qubits specified in reg to the positions specified in
perm, which is required to be a permutation of reg.
*)
let gate_permute (reg : int list) (perm : int list) =
  let rec gate_permute_helper perm_map =
    match IntMap.bindings perm_map with
    | [] -> Identity
    | (a, b) :: _ -> begin
        if a = b then
          gate_permute_helper (IntMap.remove a perm_map)
        else if not (IntMap.mem b perm_map) then
          failwith "Invalid permutation"
        else
          gate_permute_helper
            (IntMap.add a (IntMap.find b perm_map) (IntMap.remove b perm_map))
          @& Swap (a, b)
      end
  in
    if List.sort ( - ) reg <> List.sort ( - ) perm then
      failwith "Registers in permutation must have the same elements";
    let perm_map = IntMap.of_seq (List.to_seq (List.combine reg perm)) in
      gate_permute_helper perm_map

(* Left shift gate: permutes 123 into 231. *)
let rec gate_lshift (reg : int list) : gate =
  match reg with
  | [] -> Identity
  | [_] -> Identity
  | h1 :: h2 :: t -> Swap (h1, h2) @& gate_lshift (h2 :: t)

(* Right shift gate: permutes 123 into 312. *)
let gate_rshift (reg : int list) : gate = gate_adjoint (gate_lshift reg)

(* Measure and reset an entire register to the zero state. *)
let rec gate_reset_reg (reg : int list) : gate =
  match reg with
  | [] -> Identity
  | h :: t -> Reset h @& gate_reset_reg t

let rec gate_measure_reg_as_err (reg : int list) : gate =
  match reg with
  | [] -> Identity
  | h :: t -> MeasureAsErr h @& gate_measure_reg_as_err t

let rec gate_label_reg_for_potential_deletion (reg : int list) : gate =
  match reg with
  | [] -> Identity
  | h :: t ->
      PotentialDeletionLabel h @& gate_label_reg_for_potential_deletion t

let gate_semantics (u : gate) (nqubits : int) (out_reg : int list) : matrix =
  let qdim = 1 lsl nqubits in
  let rec gate_unitary_semantics (u : gate) : matrix =
    match u with
    | Identity
    | Annotation _
    | PotentialDeletionLabel _ ->
        mat_identity qdim
    | GphaseGate theta ->
        mat_scalar_mul
          (Complex.polar 1. (float_of_real theta))
          (mat_identity qdim)
    | U3Gate (i, theta, phi, lambda) -> begin
        mat_tensor
          (mat_identity (1 lsl i))
          (mat_tensor
             (mat_from_u3 (float_of_real theta) (float_of_real phi)
                (float_of_real lambda))
             (mat_identity (1 lsl (nqubits - i - 1))))
      end
    | Swap (a, b) ->
        gate_unitary_semantics (gate_cnot a b @& gate_cnot b a @& gate_cnot a b)
    | Controlled (l, bl, u0) -> begin
        let u0mat = gate_unitary_semantics u0 in
          mat_from_basis_action qdim qdim
            begin
              fun i ->
                if
                  List.fold_left ( && ) true
                    (List.map
                       (fun (j, bj) ->
                         i land (1 lsl (nqubits - 1 - j)) <> 0 == bj)
                       (List.combine l bl))
                then
                  mat_column u0mat i
                else
                  mat_column (mat_identity qdim) i
            end
      end
    | Sequence (u0, u1) ->
        gate_unitary_semantics u1 *@ gate_unitary_semantics u0
    | Reset _ -> failwith "Reset is not a unitary gate"
    | MeasureAsErr _ -> failwith "Measurement is not a unitary gate"
  in
  let rec gate_semantics_helper (u : gate) (state : matrix) : matrix =
    match u with
    | MeasureAsErr a ->
        mat_from_fun qdim qdim
          begin
            fun i j ->
              let abit = 1 lsl (nqubits - 1 - a) in
                if i land abit <> 0 || j land abit <> 0 then
                  Complex.zero
                else
                  mat_entry state i j
          end
    | Reset a ->
        mat_from_fun qdim qdim
          begin
            fun i j ->
              let abit = 1 lsl (nqubits - 1 - a) in
                if i land abit <> 0 || j land abit <> 0 then
                  Complex.zero
                else
                  Complex.add (mat_entry state i j)
                    (mat_entry state (i + abit) (j + abit))
          end
    | Sequence (u0, u1) ->
        gate_semantics_helper u1 (gate_semantics_helper u0 state)
    | _ -> begin
        let umat = gate_unitary_semantics u in
          umat *@ state *@ mat_adjoint umat
      end
  in
  let l = List.length out_reg in
  let final =
    gate_semantics_helper
      (u
      @& gate_permute (range nqubits)
           (out_reg @ int_list_diff (range nqubits) out_reg))
      (basis_column_vec qdim 0 *@ basis_row_vec qdim 0)
  in
    mat_from_fun (1 lsl l) (1 lsl l) (fun i j ->
        mat_entry final (i lsl (nqubits - l)) (j lsl (nqubits - l)))

let rec split_first_consecutive_measurements (ul : gate list) (i : int) :
    gate list * gate list =
  match ul with
  | [] -> ([], [])
  | u :: _
    when IntSet.mem i (gate_qubits_used u)
         && u <> Reset i && u <> MeasureAsErr i ->
      ([], ul)
  | u :: ul' ->
      let ul'0, ul'1 = split_first_consecutive_measurements ul' i in
        (u :: ul'0, ul'1)

(*
Given gate list ul, returns:
Part of ul up to and including first series of consecutive measurements of i
Part of ul after first series of consecutive measurements of i
*)
let rec split_up_to_first_measurements (ul : gate list) (i : int) :
    gate list * gate list =
  match ul with
  | [] -> ([], [])
  | Reset j :: _ when j = i -> split_first_consecutive_measurements ul i
  | MeasureAsErr j :: _ when j = i -> split_first_consecutive_measurements ul i
  | u :: ul' ->
      let ul'0, ul'1 = split_up_to_first_measurements ul' i in
        (u :: ul'0, ul'1)

let rec is_valid_to_delete (ul : gate list) (i : int) =
  match ul with
  | [] -> true
  | u :: ul' ->
      if is_valid_to_delete ul' i = false then
        false
      else begin
        match u with
        | _ when not (IntSet.mem i (gate_qubits_used u)) -> true
        | Identity -> true
        | U3Gate _ when gate_equal u (gate_paulix i) -> true
        | U3Gate _ -> false
        | GphaseGate _ -> true
        | Reset _ -> true
        | MeasureAsErr _ -> true
        | PotentialDeletionLabel _ -> true
        | Swap _ -> false
        | Annotation _ -> true
        | Controlled (_, _, GphaseGate _) -> false
        | Controlled (l, _, u0) ->
            if List.mem i l then false else is_valid_to_delete [u0] i
        | Sequence (u0, u1) ->
            is_valid_to_delete [u0] i && is_valid_to_delete [u1] i
      end

let rec shift_deletion_labels_left_pass (ul : gate list) : gate list * bool =
  match ul with
  | [] -> ([], false)
  | Reset i :: PotentialDeletionLabel j :: PotentialDeletionLabel k :: ul' ->
      ( PotentialDeletionLabel k :: Reset i :: PotentialDeletionLabel j
        :: fst (shift_deletion_labels_left_pass ul'),
        true )
  | MeasureAsErr i
    :: PotentialDeletionLabel j
    :: PotentialDeletionLabel k
    :: ul'
    when i = j && i <> k ->
      ( PotentialDeletionLabel k :: MeasureAsErr i :: PotentialDeletionLabel j
        :: fst (shift_deletion_labels_left_pass ul'),
        true )
  | u :: PotentialDeletionLabel i :: ul' -> begin
      let do_switch =
        begin
          match u with
          | Reset j
          | MeasureAsErr j ->
              j <> i
          | PotentialDeletionLabel _ -> false
          | _ -> true
        end
      in
        if do_switch then
          ( PotentialDeletionLabel i :: u
            :: fst (shift_deletion_labels_left_pass ul'),
            true )
        else
          let ul'', changes_made = shift_deletion_labels_left_pass ul' in
            (u :: PotentialDeletionLabel i :: ul'', changes_made)
    end
  | u :: ul' ->
      let ul'', changes_made = shift_deletion_labels_left_pass ul' in
        (u :: ul'', changes_made)

let rec gate_list_shift_deletion_labels_left (ul : gate list) : gate list =
  let ul_opt, changes_made = shift_deletion_labels_left_pass ul in
    if changes_made then
      gate_list_shift_deletion_labels_left ul_opt
    else
      ul

let rec gate_optimization_pass (ul : gate list) (out_reg : int list) :
    gate list * int list * bool =
  match ul with
  | [] -> ([], out_reg, false)
  (* Cancel adjacent adjoint gates *)
  | u :: u' :: ul' when gate_is_unitary u && gate_equal u' (gate_adjoint u) ->
      let ul'', out_reg', _ = gate_optimization_pass ul' out_reg in
        (ul'', out_reg', true)
  (* Check if deletion can occur on labeled wire segment *)
  | PotentialDeletionLabel i :: ul' -> begin
      let ul0, ul1 = split_up_to_first_measurements ul i in
        if is_valid_to_delete ul0 i then
          let ul0' =
            List.filter (fun u -> not (IntSet.mem i (gate_qubits_used u))) ul0
          in
          let ul'', out_reg', _ =
            gate_optimization_pass (ul0' @ ul1) out_reg
          in
            (ul'', out_reg', true)
        else
          let ul'', out_reg', changes_made =
            gate_optimization_pass ul' out_reg
          in
            (PotentialDeletionLabel i :: ul'', out_reg', changes_made)
    end
  (* Removing physical swap gates and relabeling wires *)
  | Swap (i, j) :: ul' -> begin
      let ul'', out_reg', _ =
        gate_optimization_pass
          (List.map (fun u -> gate_rewire u [i; j] [j; i]) ul')
          (List.map
             (fun x -> if x = i then j else if x = j then i else x)
             out_reg)
      in
        (ul'', out_reg', true)
    end
  (* Commuting single-qubit gates *)
  | u0 :: u1 :: ul'
    when begin
           let i0s = IntSet.elements (gate_qubits_used u0) in
           let i1s = IntSet.elements (gate_qubits_used u1) in
             if List.length i0s = 1 && List.length i1s = 1 then
               List.hd i1s < List.hd i0s
             else
               false
         end ->
      let ul'', out_reg', _ = gate_optimization_pass ul' out_reg in
        (u1 :: u0 :: ul'', out_reg', true)
  (* Combining controlled and anti-controlled gate *)
  | Controlled (l0, bl0, u0) :: Controlled (l1, bl1, u1) :: ul'
    when gate_equal u0 u1 && List.sort ( - ) l0 = List.sort ( - ) l1 -> begin
      let lbl0 = List.combine l0 bl0 in
      let lbl1 = List.combine l1 bl1 in
      let lbl0 = List.sort (fun (i, _) (j, _) -> i - j) lbl0 in
      let lbl1 = List.sort (fun (i, _) (j, _) -> i - j) lbl1 in
      let unequal_indices =
        List.filter_map
          (fun ((i, b0), (j, b1)) ->
            assert (i = j);
            if b0 <> b1 then Some i else None)
          (List.combine lbl0 lbl1)
      in
        if List.length unequal_indices <> 1 then
          let ul'', out_reg', changes_made =
            gate_optimization_pass (Controlled (l1, bl1, u1) :: ul') out_reg
          in
            (Controlled (l0, bl0, u0) :: ul'', out_reg', changes_made)
        else
          let remove_i = List.hd unequal_indices in
          let lbl0 = List.remove_assoc remove_i lbl0 in
          let l, bl = List.split lbl0 in
          let newgate =
            if List.length l = 0 then
              u0
            else
              Controlled (l, bl, u0)
          in
          let ul'', out_reg', _ = gate_optimization_pass ul' out_reg in
            (newgate :: ul'', out_reg', true)
    end
  (* Commuting controlled gates *)
  | Controlled (l0, bl0, u0) :: Controlled (l1, bl1, u1) :: ul'
    when let gph0 = IntSet.cardinal (gate_qubits_used u0) = 0 in
         let gph1 = IntSet.cardinal (gate_qubits_used u1) = 0 in
           begin
             IntSet.for_all
               (fun x -> not (List.mem x l1))
               (gate_qubits_used u0)
             && IntSet.for_all
                  (fun x -> not (List.mem x l0))
                  (gate_qubits_used u1)
           end
           && ((gph0 && not gph1)
              || (not (gph1 && not gph0))
                 && List.hd (List.sort ( - ) l1) < List.hd (List.sort ( - ) l0)
              ) ->
      let ul'', out_reg', _ =
        gate_optimization_pass (Controlled (l0, bl0, u0) :: ul') out_reg
      in
        (Controlled (l1, bl1, u1) :: ul'', out_reg', true)
  | u :: ul' ->
      let ul'', out_reg', changes_made = gate_optimization_pass ul' out_reg in
        (u :: ul'', out_reg', changes_made)

let optimization_print = ref false
let optimization_max_length = 10000

let rec gate_list_optimize (ul : gate list) (out_reg : int list) :
    gate list * int list =
  if !optimization_print then
    Printf.printf ".%!";
  let ul = gate_list_shift_deletion_labels_left ul in
  let ul_opt, out_reg', changes_made = gate_optimization_pass ul out_reg in
    if changes_made then
      gate_list_optimize ul_opt out_reg'
    else
      (ul, out_reg')

let gate_optimize (u : gate) (out_reg : int list) : gate * int list =
  let ul = gate_to_list u in
    if List.length ul > optimization_max_length then begin
      if !optimization_print then
        Printf.printf "Circuit too large - not optimizing\n%!";
      (u, out_reg)
    end
    else begin
      if !optimization_print then
        Printf.printf "Optimizing\n%!";
      let ul, out_reg = gate_list_optimize ul out_reg in
        if !optimization_print then
          Printf.printf "\n%!";
        (gate_of_list ul, out_reg)
    end
