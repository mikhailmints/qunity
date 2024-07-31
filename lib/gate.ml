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
      l0 = l1 && bl0 = bl1 && gate_equal u0 u1
  | Sequence (u00, u01), Sequence (u10, u11) ->
      gate_equal u00 u10 && gate_equal u01 u11
  | _ -> u0 = u1

let rec gate_qubits_used (u : gate) : IntSet.t =
  match u with
  | Identity -> IntSet.empty
  | U3Gate (i, _, _, _) -> IntSet.singleton i
  | GphaseGate _ -> IntSet.empty
  | Reset i -> IntSet.singleton i
  | MeasureAsErr i -> IntSet.singleton i
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

let rec gate_optimization_pass (ul : gate list) : gate list * bool =
  match ul with
  | [] -> ([], false)
  | u :: u' :: ul' when gate_is_unitary u && gate_equal u' (gate_adjoint u) ->
      (fst (gate_optimization_pass ul'), true)
  | u :: ul' ->
      let ul'', changes_made = gate_optimization_pass ul' in
        (u :: ul'', changes_made)

let rec gate_list_optimize (ul : gate list) : gate list =
  let ul_opt, changes_made = gate_optimization_pass ul in
    if changes_made then
      gate_list_optimize ul_opt
    else
      ul

let gate_optimize (u : gate) : gate =
  gate_of_list (gate_list_optimize (gate_to_list u))

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

let gate_semantics (u : gate) (nqubits : int) (out_reg : int list) : matrix =
  let qdim = 1 lsl nqubits in
  let rec gate_unitary_semantics (u : gate) : matrix =
    match u with
    | Identity
    | Annotation _ ->
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
