open Util
open Reals
open Matrix
open Gate

(** Computes the semantics of a single unitary gate, outputting a unitary
    matrix. *)
let rec gate_unitary_semantics (nqubits : int) (u : gate) : matrix =
  let qdim = 1 lsl nqubits in
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
        let single_qubit_gate =
          if u = gate_paulix i then
            mat_xgate
          else
            mat_from_u3 (float_of_real theta) (float_of_real phi)
              (float_of_real lambda)
        in
          mat_tensor
            (mat_identity (1 lsl i))
            (mat_tensor single_qubit_gate
               (mat_identity (1 lsl (nqubits - i - 1))))
      end
    | Swap (a, b) ->
        gate_unitary_semantics nqubits
          (gate_cnot a b @& gate_cnot b a @& gate_cnot a b)
    | Controlled (l, bl, u0) -> begin
        let u0mat = gate_unitary_semantics nqubits u0 in
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
        gate_unitary_semantics nqubits u1 *@ gate_unitary_semantics nqubits u0
    | Reset _ -> failwith "Reset is not a unitary gate"
    | MeasureAsErr _ -> failwith "Measurement is not a unitary gate"

(** Computes the semantics of a gate acting on the initial state
    {m |0 \rangle\langle 0|}, outputting the final density matrix. *)
let gate_semantics (u : gate) (nqubits : int) (out_reg : int list) : matrix =
  let qdim = 1 lsl nqubits in
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
        let umat = gate_unitary_semantics nqubits u in
          umat *@ state *@ mat_adjoint umat
      end
  in
  let u =
    u
    @& gate_permute (range nqubits)
         (out_reg @ int_list_diff (range nqubits) out_reg)
  in
  let cl =
    Array.of_list (List.map (fun _ -> Classical false) (range nqubits))
  in
  let err = ref (Classical false) in
  let _ = gate_classical_propagation (gate_to_list u) cl err in
  let len = List.length out_reg in
  let cl, _ = list_split_at_i (Array.to_list cl) len in
    begin
      if List.for_all (fun x -> x <> Quantum) cl && !err <> Quantum then
        let state_i =
          List.fold_left ( + ) 0
            (List.mapi
               begin
                 fun i x ->
                   match x with
                   | Classical b -> if b then 1 lsl (len - 1 - i) else 0
                   | Quantum -> failwith "unreachable"
               end
               cl)
        in
          mat_from_map (1 lsl len) (1 lsl len)
            (if !err = Classical true then
               Int2Map.empty
             else
               Int2Map.singleton (state_i, state_i) Complex.one)
      else
        let all_qubit_state =
          gate_semantics_helper u
            (basis_column_vec qdim 0 *@ basis_row_vec qdim 0)
        in
          mat_from_fun (1 lsl len) (1 lsl len) (fun i j ->
              mat_entry all_qubit_state
                (i lsl (nqubits - len))
                (j lsl (nqubits - len)))
    end
