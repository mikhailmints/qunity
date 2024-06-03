open Driver_util
open Qunity_prototypes
open Util
open Reals
open Compilation

let rec gate_to_qiskit_gate (u : gate) (nqubits : int)
    (gatenames : StringSet.t) : string * int list * StringSet.t =
  match u with
  | Identity -> failwith "Identities should be removed"
  | PauliX i -> ("XGate()", [i], gatenames)
  | Had i -> ("HGate()", [i], gatenames)
  | U3Gate (i, theta, phi, lambda) ->
      let s =
        Printf.sprintf "U3Gate(%s, %s, %s)"
          (python_string_of_real theta)
          (python_string_of_real phi)
          (python_string_of_real lambda)
      in
        (s, [i], gatenames)
  | GphaseGate (l, theta) ->
      let label = fresh_string "gphase_" gatenames in
      let s =
        Printf.sprintf
          "QuantumCircuit(%d, global_phase=%s, name=\"%s\").to_gate()"
          (List.length l)
          (python_string_of_real theta)
          label
      in
        (s, l, StringSet.add label gatenames)
  | Reset i -> ("Reset()", [i], gatenames)
  | Swap (i, j) -> ("SwapGate()", [i; j], gatenames)
  | Annotation (l, name) -> begin
      let s =
        Printf.sprintf "QuantumCircuit(%d, name=\"%s\").to_gate()"
          (List.length l) name
      in
        (s, l, gatenames)
    end
  | Controlled (l, bl, u0) -> begin
      let u0_gate, u0_l, gatenames =
        gate_to_qiskit_gate u0 nqubits gatenames
      in
      let s =
        Printf.sprintf "%s.control(num_ctrl_qubits=%d, ctrl_state=\"%s\")"
          u0_gate (List.length l)
          (List.fold_left (fun b b' -> b ^ if b' then "1" else "0") "" bl)
      in
        (s, l @ u0_l, gatenames)
    end
  | Sequence (u0, u1) -> begin
      let u0_gate, u0_l, gatenames =
        gate_to_qiskit_gate u0 nqubits gatenames
      in
      let u1_gate, u1_l, gatenames =
        gate_to_qiskit_gate u1 nqubits gatenames
      in
      let label = fresh_string "_" gatenames in
      let s =
        Printf.sprintf "gate_sequence(%s, %s, %s, %s, \"%s\")" u0_gate
          (string_of_list string_of_int u0_l)
          u1_gate
          (string_of_list string_of_int u1_l)
          label
      in
        (s, int_list_union u0_l u1_l, StringSet.add label gatenames)
    end

let gate_to_qiskit_file (u : gate) (nqubits : int) (out_reg : int list)
    (flag_reg : int list) : string =
  let header = read_file "bin/qiskit_header.py" in
  let gate_str, gate_indices, gatenames =
    if u = Identity then
      ("None", [], StringSet.empty)
    else
      gate_to_qiskit_gate u nqubits StringSet.empty
  in
    header
    ^ Printf.sprintf "build_circuit(%d, %s, %s, %s, %s, %s)\n" nqubits
        (string_of_list string_of_int out_reg)
        (string_of_list string_of_int flag_reg)
        gate_str
        (string_of_list string_of_int gate_indices)
        (string_of_list
           (fun x -> "\"" ^ x ^ "\"")
           (List.of_seq (StringSet.to_seq gatenames)))

let () =
  let prog_filename = Sys.argv.(1) in
  let out_filename = Sys.argv.(2) in
  let annotate = bool_of_string Sys.argv.(3) in
    compile_file prog_filename out_filename gate_to_qiskit_file annotate
