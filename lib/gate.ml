open Util
open Reals

(** Low-level representation of a quantum circuit. Describes unitary gates (or
    reset or measurement operations) acting on qubits specified by integer
    labels. *)
type gate =
  | Identity
  | U3Gate of (int * real * real * real)
  | GphaseGate of real
  | Reset of int
  | MeasureAsErr of int
  | PotentialDeletionLabel of int
  | ZeroStateLabel of int
  | Swap of int * int
  | Annotation of int list * string
  | Controlled of (int list * bool list * gate)
  | Sequence of (gate * gate)

(** Alias for [Sequence]. *)
let ( @& ) a b = Sequence (a, b)

(** String representation of a gate. *)
let rec string_of_gate (u : gate) : string =
  match u with
  | Identity -> "Identity"
  | U3Gate (i, theta, phi, lambda) ->
      Printf.sprintf "U3Gate (%d, %s, %s, %s)" i (string_of_real theta)
        (string_of_real phi) (string_of_real lambda)
  | GphaseGate theta -> Printf.sprintf "GphaseGate %s" (string_of_real theta)
  | Reset i -> Printf.sprintf "Reset %d" i
  | MeasureAsErr i -> Printf.sprintf "MeasureAsErr %d" i
  | PotentialDeletionLabel i -> Printf.sprintf "PotentialDeletionLabel %d" i
  | ZeroStateLabel i -> Printf.sprintf "ZeroStateLabel %d" i
  | Swap (i, j) -> Printf.sprintf "Swap (%d, %d)" i j
  | Annotation (l, s) ->
      Printf.sprintf "Annotation (%s, %s)" (string_of_list string_of_int l) s
  | Controlled (l, bl, u0) ->
      Printf.sprintf "Controlled (%s, %s, %s)"
        (string_of_list string_of_int l)
        (string_of_list string_of_bool bl)
        (string_of_gate u0)
  | Sequence (u0, u1) ->
      Printf.sprintf "Sequence (%s, %s)" (string_of_gate u0)
        (string_of_gate u1)

(** A type used to track whether a qubit is in a classical state during
    optimization passes. *)
type classical_prop_state = Classical of bool | Quantum

(** The Pauli {m X} gate. *)
let gate_paulix (i : int) : gate = U3Gate (i, Pi, Const 0, Pi)

(** The Pauli {m Z} gate. *)
let gate_pauliz (i : int) : gate = U3Gate (i, Const 0, Pi, Const 0)

(** The Hadamard gate. *)
let gate_had (i : int) : gate = U3Gate (i, Div (Pi, Const 2), Const 0, Pi)

(** The CNOT gate. *)
let gate_cnot (b0 : int) (b1 : int) : gate =
  Controlled ([b0], [true], gate_paulix b1)

(** Checks whether the gate is unitary (all are, except for [Reset] and
    [MeasureAsErr]). *)
let gate_is_unitary (u : gate) : bool =
  match u with
  | Reset _ -> false
  | MeasureAsErr _ -> false
  | _ -> true

(** Takes the adjoint of a unitary gate. *)
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
  | PotentialDeletionLabel _
  | ZeroStateLabel _ ->
      Identity
  | Swap (i, j) -> Swap (i, j)
  | Annotation (l, s) -> Annotation (l, s)
  | Controlled (l, bl, u0) -> Controlled (l, bl, gate_adjoint u0)
  | Sequence (u0, u1) -> Sequence (gate_adjoint u1, gate_adjoint u0)

(** These are some sufficient (but not necessary) conditions for two gates to
    be equal. *)
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

(** Whether the given gate is a Pauli {m X} gate. *)
let gate_is_x (u : gate) : bool =
  match u with
  | U3Gate (i, _, _, _) -> gate_equal u (gate_paulix i)
  | _ -> false

(** Whether the given gate is a global phase gate. *)
let gate_is_gphase (u : gate) : bool =
  match u with
  | GphaseGate _ -> true
  | _ -> false

(** Given a gate, outputs the set of qubits used by the gate. *)
let rec gate_qubits_used (u : gate) : IntSet.t =
  match u with
  | Identity -> IntSet.empty
  | U3Gate (i, _, _, _) -> IntSet.singleton i
  | GphaseGate _ -> IntSet.empty
  | Reset i
  | MeasureAsErr i
  | PotentialDeletionLabel i
  | ZeroStateLabel i ->
      IntSet.singleton i
  | Swap (i, j) -> IntSet.of_list [i; j]
  | Annotation (l, _) -> IntSet.of_list l
  | Controlled (l, _, u0) ->
      IntSet.union (IntSet.of_list l) (gate_qubits_used u0)
  | Sequence (u0, u1) ->
      IntSet.union (gate_qubits_used u0) (gate_qubits_used u1)

(** Given a gate, outputs the number of qubits necessary to apply the gate -
    that is, the maximum qubit index used by the gate plus one. *)
let gate_num_qubits (u : gate) : int =
  let max_index = List.fold_left max 0 (IntSet.to_list (gate_qubits_used u)) in
    max_index + 1

(** The number of [MeasureAsErr] gates in the given gate. *)
let rec gate_num_err_measurements (u : gate) : int =
  match u with
  | MeasureAsErr _ -> 1
  | Sequence (u0, u1) ->
      gate_num_err_measurements u0 + gate_num_err_measurements u1
  | _ -> 0

(** Rewires a gate by replacing all references to certain qubits with other
    qubits. *)
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
    | ZeroStateLabel i -> ZeroStateLabel (int_map_find_or_keep rewiring i)
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

(** Removes all identity gates from a circuit, unless the entire circuit is
    equivalent to an identity, in which case this returns the identity gate. *)
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

(** Replaces controlled sequences of gates with sequences of controlled gates,
    making the only controlled gates in the end be single-qubit gates, global
    phase gates, and swap gates. *)
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

(** Converts a gate into a list, eliminating all uses of [Sequence]. *)
let rec gate_to_list (u : gate) : gate list =
  match u with
  | Sequence (u0, u1) -> gate_to_list u0 @ gate_to_list u1
  | _ -> [u]

(** Converts a list of gates into a single gate, joining it using [Sequence].
*)
let rec gate_of_list (ul : gate list) : gate =
  match ul with
  | [] -> Identity
  | [u] -> u
  | u :: ul' -> Sequence (u, gate_of_list ul')

(** Shares a register to another one by applying CNOT gates for each qubit. *)
let rec gate_share (reg0 : int list) (reg1 : int list) : gate =
  match (reg0, reg1) with
  | [], [] -> Identity
  | h1 :: t1, h2 :: t2 -> gate_cnot h1 h2 @& gate_share t1 t2
  | _ -> invalid_arg "Registers must be of the same size"

(** Swaps two registers of equal size. *)
let rec gate_swap_regs (reg0 : int list) (reg1 : int list) : gate =
  match (reg0, reg1) with
  | [], [] -> Identity
  | h1 :: t1, h2 :: t2 -> Swap (h1, h2) @& gate_swap_regs t1 t2
  | _ -> invalid_arg "Registers must be of the same size"

(** A gate that sends the qubits specified in [reg] to the positions specified
    in [perm], which is required to be a permutation of [reg]. *)
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

(** Left shift gate: permutes [123] into [231]. *)
let rec gate_lshift (reg : int list) : gate =
  match reg with
  | [] -> Identity
  | [_] -> Identity
  | h1 :: h2 :: t -> Swap (h1, h2) @& gate_lshift (h2 :: t)

(** Right shift gate: permutes [123] into [312]. *)
let gate_rshift (reg : int list) : gate = gate_adjoint (gate_lshift reg)

(** Measures and resets an entire register to the {m |0\rangle} state. *)
let rec gate_reset_reg (reg : int list) : gate =
  match reg with
  | [] -> Identity
  | h :: t -> Reset h @& gate_reset_reg t

(** Measures an entire register as a flag register. *)
let rec gate_measure_reg_as_err (reg : int list) : gate =
  match reg with
  | [] -> Identity
  | h :: t -> MeasureAsErr h @& gate_measure_reg_as_err t

(** Adds [PotentialDeletionLabel]'s to an entire register. *)
let rec gate_label_reg_for_potential_deletion (reg : int list) : gate =
  match reg with
  | [] -> Identity
  | h :: t ->
      PotentialDeletionLabel h @& gate_label_reg_for_potential_deletion t

(** Adds [ZeroStateLabel]'s to an entire register. *)
let rec gate_label_reg_as_zero_state (reg : int list) : gate =
  match reg with
  | [] -> Identity
  | h :: t -> ZeroStateLabel h @& gate_label_reg_as_zero_state t

(** Splits an input gate list [ul] into two parts, where the first part is as
    large as possible subject to the restriction that any gates in it involving
    the qubit [i] can only be measurements or zero state labels. *)
let rec split_first_consecutive_measurements (ul : gate list) (i : int) :
    gate list * gate list =
  match ul with
  | [] -> ([], [])
  | u :: _
    when IntSet.mem i (gate_qubits_used u)
         && u <> Reset i && u <> MeasureAsErr i && u <> ZeroStateLabel i ->
      ([], ul)
  | u :: ul' ->
      let ul'0, ul'1 = split_first_consecutive_measurements ul' i in
        (u :: ul'0, ul'1)

(** Given gate list [ul], returns the part of [ul] up to and including the
    first series of consecutive measurements or zero state labels of [i], and
    the remaining part. *)
let rec split_up_to_first_measurements (ul : gate list) (i : int) :
    gate list * gate list =
  match ul with
  | [] -> ([], [])
  | Reset j :: _
  | MeasureAsErr j :: _
    when j = i ->
      split_first_consecutive_measurements ul i
  | u :: ul' ->
      let ul'0, ul'1 = split_up_to_first_measurements ul' i in
        (u :: ul'0, ul'1)

(** Determines whether or not it is valid to delete all the gates acting on
    qubit [i] in the given list [ul], provided that the qubit [i] starts in the
    {m |0\rangle} state and is measured at the end. *)
let rec is_valid_to_delete (ul : gate list) (i : int) =
  match ul with
  | [] ->
      false
      (* This could only have been reached if the qubit of interest is an output qubit *)
  | u :: ul' ->
      if is_valid_to_delete ul' i = false then
        false
      else begin
        match u with
        | _ when not (IntSet.mem i (gate_qubits_used u)) -> true
        | U3Gate _ when gate_equal u (gate_paulix i) -> true
        | U3Gate _ -> false
        | Swap _ -> false
        | Controlled (_, _, GphaseGate _) -> false
        | Controlled (l, _, u0) ->
            if List.mem i l then false else is_valid_to_delete [u0] i
        | Sequence _ -> failwith "Sequences should not be present"
        | _ -> true
      end

(** Splits the gate list [ul] into the parts before and after the first CNOT
    gate with [i] as a target, also returning the control qubit of that CNOT
    gate. If there is no CNOT gate, this returns [None]. *)
let rec split_up_to_first_cnot (ul : gate list) (i : int) :
    (int * gate list * gate list) option =
  match ul with
  | [] -> None
  | Reset i' :: _ when i' = i -> None
  | MeasureAsErr i' :: _ when i' = i -> None
  | Controlled ([j], [true], u0) :: ul' when u0 = gate_paulix i ->
      Some (j, [], ul')
  | u :: ul' -> (
      match split_up_to_first_cnot ul' i with
      | None -> None
      | Some (j, ul0, ul1) -> Some (j, u :: ul0, ul1))

(** Checks whether a gate list before a CNOT gate with the target qubit [i] is
    valid for the purposes of removing an unnecessary share operation. *)
let rec is_valid_before_share (ul : gate list) (i : int) =
  match ul with
  | [] -> true
  | u :: ul' ->
      if is_valid_before_share ul' i = false then
        false
      else begin
        match u with
        | _ when not (IntSet.mem i (gate_qubits_used u)) -> true
        | U3Gate _ -> false
        | Swap _ -> false
        | Controlled (l, _, _) -> List.mem i l
        | Sequence _ -> failwith "Sequences should not be present"
        | _ -> true
      end

(** Checks whether a gate list between two CNOT gates with the target qubit [i]
    is valid for the purposes of removing an unnecessary share operation. *)
let rec is_valid_during_share (ul : gate list) (i : int) (j : int)
    (i_flipped : bool) (j_flipped : bool) =
  match ul with
  | [] -> (not i_flipped) && not j_flipped
  | u :: ul' ->
      let i_flipped, j_flipped =
        match u with
        | U3Gate _ when u = gate_paulix i -> (not i_flipped, j_flipped)
        | U3Gate _ when u = gate_paulix j -> (i_flipped, not j_flipped)
        | _ -> (i_flipped, j_flipped)
      in
        if is_valid_during_share ul' i j i_flipped j_flipped = false then
          false
        else begin
          match u with
          | _
            when (not (IntSet.mem i (gate_qubits_used u)))
                 && not (IntSet.mem j (gate_qubits_used u)) ->
              true
          | _
            when IntSet.mem i (gate_qubits_used u)
                 && IntSet.mem j (gate_qubits_used u) ->
              false
          | U3Gate _ when u = gate_paulix i || u = gate_paulix j -> true
          | U3Gate _
          | Reset _
          | MeasureAsErr _
          | Swap _ ->
              false
          | Controlled (_, _, u0) ->
              (not (IntSet.mem i (gate_qubits_used u0)))
              && not (IntSet.mem j (gate_qubits_used u0))
          | Sequence _ -> failwith "Sequences should not be present"
          | _ -> true
        end

(** Attempts to find a region where an unnecessary share operation is applied
    in the list [l] with qubit [i] as a target. If a region is found, the
    source qubit, as well as the regions before, during, and after the sharing
    are returned. Otherwise, [None] is returned. *)
let find_disentangling_region (ul : gate list) (i : int) :
    (int * gate list * gate list * gate list) option =
  match split_up_to_first_cnot ul i with
  | None -> None
  | Some (j, before, during_after) -> begin
      match split_up_to_first_cnot during_after i with
      | None -> None
      | Some (j', during, after) -> begin
          if
            j = j'
            && is_valid_before_share before i
            && is_valid_during_share during i j false false
          then
            Some (j, before, during, after)
          else
            None
        end
    end

(** A pass that shifts all [PotentialDeletionLabel]'s in a gate list towards
    the left, until they hit a measurement or zero state label of the same
    qubit. *)
let rec shift_deletion_labels_left_pass (ul : gate list) : gate list * bool =
  match ul with
  | [] -> ([], false)
  | u :: PotentialDeletionLabel i :: PotentialDeletionLabel j :: ul'
    when (u = Reset i || u = MeasureAsErr i || u = ZeroStateLabel i) && i <> j
    ->
      ( PotentialDeletionLabel j :: u :: PotentialDeletionLabel i
        :: fst (shift_deletion_labels_left_pass ul'),
        true )
  | u :: PotentialDeletionLabel i :: ul' -> begin
      let do_switch =
        begin
          match u with
          | Reset j
          | MeasureAsErr j
          | ZeroStateLabel j ->
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

(** Shifts all [PotentialDeletionLabel]'s in a gate list as far left as
    possible, until they hit a measurement of the same qubit. *)
let rec gate_list_shift_deletion_labels_left (ul : gate list) : gate list =
  let ul_opt, changes_made = shift_deletion_labels_left_pass ul in
    if changes_made then
      gate_list_shift_deletion_labels_left ul_opt
    else
      ul

(** Iterates though the given gate list, keeping track of qubits that are in a
    classical state and simplifying gates where possible. *)
let rec gate_classical_propagation (ul : gate list)
    (cl : classical_prop_state array) (err : classical_prop_state ref) :
    gate list =
  match ul with
  | [] -> []
  | u :: ul' -> begin
      let u' =
        match u with
        | Identity
        | GphaseGate _
        | PotentialDeletionLabel _
        | Annotation _ ->
            u
        | MeasureAsErr i -> begin
            match (cl.(i), !err) with
            | Classical true, _ ->
                err := Classical true;
                u
            | Quantum, Classical false ->
                err := Quantum;
                u
            | Classical false, _ -> Identity
            | _ -> u
          end
        | Sequence _ -> failwith "Sequences should not be present"
        | U3Gate (i, _, _, _) when u = gate_paulix i -> begin
            match cl.(i) with
            | Classical b ->
                cl.(i) <- Classical (not b);
                u
            | Quantum -> u
          end
        | U3Gate (i, _, _, _) ->
            cl.(i) <- Quantum;
            u
        | Reset i -> begin
            match cl.(i) with
            | Classical false -> Identity
            | _ ->
                cl.(i) <- Classical false;
                u
          end
        | ZeroStateLabel i ->
            cl.(i) <- Classical false;
            u
        | Swap (i, j) -> begin
            let temp = cl.(i) in
              cl.(i) <- cl.(j);
              cl.(j) <- temp;
              u
          end
        | Controlled (_, _, Swap (i, j))
          when cl.(i) = cl.(j) && cl.(i) <> Quantum ->
            Identity
        | Controlled (l, bl, u0) -> begin
            let l_states = List.map (fun i -> cl.(i)) l in
            let is_active =
              List.for_all
                (fun (state, b) -> state = Quantum || state = Classical b)
                (List.combine l_states bl)
            in
              if not is_active then
                Identity
              else if List.for_all (fun state -> state <> Quantum) l_states
              then
                List.hd (gate_classical_propagation [u0] cl err)
              else begin
                let to_keep =
                  List.filter
                    (fun (state, (_, b)) -> state <> Classical b)
                    (List.combine l_states (List.combine l bl))
                in
                let l', bl' = List.split (List.map snd to_keep) in
                  IntSet.iter
                    (fun i -> cl.(i) <- Quantum)
                    (gate_qubits_used u0);
                  Controlled (l', bl', u0)
              end
          end
      in
        match (u', ul') with
        | Identity, ul' -> gate_classical_propagation ul' cl err
        | _ -> u' :: gate_classical_propagation ul' cl err
    end

(** Checks if two gates can be cancelled either completely or into one smaller
    gate. *)
let gate_cancellation (u : gate) (u' : gate) : gate option =
  match (u, u') with
  | _ when gate_is_unitary u && gate_equal u' (gate_adjoint u) -> Some Identity
  | Controlled (l0, bl0, u0), Controlled (l1, bl1, u1)
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
          None
        else
          let remove_i = List.hd unequal_indices in
          let lbl0 = List.remove_assoc remove_i lbl0 in
          let l, bl = List.split lbl0 in
            if List.length l = 0 then
              Some u0
            else
              Some (Controlled (l, bl, u0))
    end
  | _ -> None

(** Commutes one gate to the right through the given list, until it cancels
    with something or it can't commute further. If it cancels, this returns the
    updated list. Otherwise this returns [None]. *)
let rec gate_commutation_pass (ul : gate list) : gate list option =
  match ul with
  | []
  | [_] ->
      None
  | a :: b :: ul' -> begin
      match gate_cancellation a b with
      | Some Identity -> Some ul'
      | Some u' -> Some (u' :: ul')
      | None -> begin
          let commuted =
            begin
              match (a, b) with
              | _
                when IntSet.inter (gate_qubits_used a) (gate_qubits_used b)
                     = IntSet.empty ->
                  Some (b, a)
              | _, GphaseGate _
              | GphaseGate _, _
              | _, Annotation _
              | Annotation _, _ ->
                  Some (b, a)
              | U3Gate (i, _, _, _), Controlled (_, _, u0)
                when a = gate_paulix i && u0 = gate_paulix i ->
                  Some (b, a)
              | Controlled (_, _, u0), U3Gate (i, _, _, _)
                when b = gate_paulix i && u0 = gate_paulix i ->
                  Some (b, a)
              | U3Gate (i, _, _, _), Controlled (l, bl, u0)
                when a = gate_paulix i
                     && List.mem i l
                     && not (List.length l = 1 && gate_is_gphase u0) -> begin
                  let ind = list_index ( = ) l i in
                  let bl0, bl1 = list_split_at_i bl ind in
                    Some
                      ( Controlled
                          (l, bl0 @ (not (List.hd bl1) :: List.tl bl1), u0),
                        a )
                end
              | Controlled (l, bl, u0), U3Gate (i, _, _, _)
                when b = gate_paulix i
                     && List.mem i l
                     && not (List.length l = 1 && gate_is_gphase u0) -> begin
                  let ind = list_index ( = ) l i in
                  let bl0, bl1 = list_split_at_i bl ind in
                    Some
                      ( b,
                        Controlled
                          (l, bl0 @ (not (List.hd bl1) :: List.tl bl1), u0) )
                end
              | Controlled (l0, bl0, u0), Controlled (l1, bl1, u1)
                when ((gate_is_x u0 && gate_is_x u1)
                     || begin
                          let map0 = IntMap.of_list (List.combine l0 bl0) in
                          let map1 = IntMap.of_list (List.combine l1 bl1) in
                            IntMap.cardinal
                              (IntMap.filter
                                 (fun i b ->
                                   IntMap.find_opt i map1 = Some (not b))
                                 map0)
                            > 0
                        end)
                     && IntSet.for_all
                          (fun x -> not (List.mem x l1))
                          (gate_qubits_used u0)
                     && IntSet.for_all
                          (fun x -> not (List.mem x l0))
                          (gate_qubits_used u1) ->
                  Some (b, a)
              | _ -> None
            end
          in
            match commuted with
            | None -> None
            | Some (b', a') -> begin
                match gate_commutation_pass (a' :: ul') with
                | None -> None
                | Some ul'' -> Some (b' :: ul'')
              end
        end
    end

(** An optimization pass that iterates though the given gate list [ul], making
    various gate simplifications. This can also modify the resulting [out_reg]
    when it eliminates SWAP gates. *)
let rec gate_optimization_pass (ul : gate list) (out_reg : int list) :
    gate list * int list * bool =
  match (ul, gate_commutation_pass ul) with
  | [], _ -> ([], out_reg, false)
  | _, Some [] -> ([], out_reg, true)
  | _, Some ul' -> begin
      let ul'', out_reg', _ = gate_optimization_pass ul' out_reg in
        (ul'', out_reg', true)
    end
  (* Remove global phases *)
  | GphaseGate _ :: ul', _ ->
      let ul'', out_reg', _ = gate_optimization_pass ul' out_reg in
        (ul'', out_reg', true)
  (* Check if deletion can occur on labeled wire segment *)
  | PotentialDeletionLabel i :: ul', _ -> begin
      let ul0, ul1 = split_up_to_first_measurements ul i in
        if is_valid_to_delete ul0 i then
          let ul0' =
            List.filter (fun u -> not (IntSet.mem i (gate_qubits_used u))) ul0
          in
          let ul'', out_reg', _ =
            gate_optimization_pass (ul0' @ ul1) out_reg
          in
            (ul'', out_reg', true)
        else begin
          match find_disentangling_region ul0 i with
          | Some (j, before, during, after) -> begin
              let ul0' =
                before
                @ List.map (fun x -> gate_rewire x [i] [j]) during
                @ after
              in
              let ul'', out_reg', _ =
                gate_optimization_pass (ul0' @ ul1) out_reg
              in
                (ul'', out_reg', true)
            end
          | None ->
              let ul'', out_reg', changes_made =
                gate_optimization_pass ul' out_reg
              in
                (PotentialDeletionLabel i :: ul'', out_reg', changes_made)
        end
    end
  (* Removing physical swap gates and relabeling wires *)
  | Swap (i, j) :: ul', _ -> begin
      let ul'', out_reg', _ =
        gate_optimization_pass
          (List.map (fun u -> gate_rewire u [i; j] [j; i]) ul')
          (List.map
             (fun x -> if x = i then j else if x = j then i else x)
             out_reg)
      in
        (ul'', out_reg', true)
    end
  | u :: ul', _ ->
      let ul'', out_reg', changes_made = gate_optimization_pass ul' out_reg in
        (u :: ul'', out_reg', changes_made)

(** Whether to print messages during the gate optimization procedure. *)
let optimization_print = ref false

(** Runs the optimization procedure on a given gate list. *)
let rec gate_list_optimize (ul : gate list) (out_reg : int list)
    (nqubits : int) : gate list * int list =
  if !optimization_print then
    Printf.printf ".%!";
  let ul =
    gate_classical_propagation ul
      (Array.of_list (List.map (fun _ -> Classical false) (range nqubits)))
      (ref (Classical false))
  in
  let ul = gate_list_shift_deletion_labels_left ul in
  let ul_opt, out_reg', changes_made = gate_optimization_pass ul out_reg in
    if changes_made then
      gate_list_optimize ul_opt out_reg' nqubits
    else
      (* If no changes were made in the last pass, remove all labels
         and try one more time. *)
      let ul_no_labels =
        List.filter
          (fun u ->
            match u with
            | PotentialDeletionLabel _
            | ZeroStateLabel _ ->
                false
            | _ -> true)
          ul
      in
      (* Replace anti-controlled gphase with controlled, surrounded by
         X gates (since that is how it will be output in QASM) before
         optimizing further. *)
      let ul_no_labels =
        List.flatten
          (List.map
             (fun u ->
               match u with
               | Controlled ([i], [false], GphaseGate theta) ->
                   [
                     gate_paulix i;
                     Controlled ([i], [true], GphaseGate theta);
                     gate_paulix i;
                   ]
               | _ -> [u])
             ul_no_labels)
      in
        if ul_no_labels <> ul then
          gate_list_optimize ul_no_labels out_reg nqubits
        else
          (ul, out_reg)

(** The main entry point for the gate optimization procedure. *)
let gate_optimize (u : gate) (out_reg : int list) : gate * int list =
  let ul = gate_to_list u in
  let nqubits = gate_num_qubits u in
    if !optimization_print then
      Printf.printf "Optimizing\n%!";
    let ul, out_reg = gate_list_optimize ul out_reg nqubits in
      if !optimization_print then
        Printf.printf "\n%!";
      (gate_of_list ul, out_reg)
