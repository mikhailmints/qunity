open Driver_util
open Qunity_prototypes
open Util
open Reals
open Extended_syntax
open Compilation

let rec unitary_to_qiskit_gate (u : unitary) (nqubits : int)
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
      let s =
        Printf.sprintf
          "QuantumCircuit(%d, global_phase=%s, \
           name=\"gphase(%.2f)\").to_gate()"
          (List.length l)
          (python_string_of_real theta)
          (float_of_real theta)
      in
        (s, l, gatenames)
  | Controlled (l, bl, u0) -> begin
      let u0_gate, u0_l, gatenames =
        unitary_to_qiskit_gate u0 nqubits gatenames
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
        unitary_to_qiskit_gate u0 nqubits gatenames
      in
      let u1_gate, u1_l, gatenames =
        unitary_to_qiskit_gate u1 nqubits gatenames
      in
      let label = fresh_string gatenames in
      let s =
        Printf.sprintf "gate_sequence(%s, %s, %s, %s, \"%s\", %d)" u0_gate
          (string_of_list string_of_int u0_l)
          u1_gate
          (string_of_list string_of_int u1_l)
          label nqubits
      in
        (s, range nqubits, StringSet.add label gatenames)
    end

let unitary_to_qiskit_file (gate : unitary) (nqubits : int)
    (out_reg : int list) =
  let header = read_file "bin/qiskit_header.py" in
  let circ_decl =
    Printf.sprintf
      "\n\n\
       cr = ClassicalRegister(%d)\n\
       circuit = QuantumCircuit(QuantumRegister(%d), cr)\n\n"
      (List.length out_reg) nqubits
  in
  let footer =
    Printf.sprintf
      "circuit.measure(%s, cr)\n\n\
       circuit.draw(\"mpl\", filename=__file__.replace(\".py\", \".png\"))\n\n\
       qasm3.dump(circuit, open(__file__.replace(\".py\", \".qasm\"), \"w\"))\n"
      (string_of_list string_of_int out_reg)
  in
    if gate = Identity then
      header ^ circ_decl ^ footer
    else
      let gate_str, gate_l, gatenames =
        unitary_to_qiskit_gate gate nqubits StringSet.empty
      in
        header ^ circ_decl
        ^ Printf.sprintf
            "circuit.append(%s, %s)\n\n\
             circuit = circuit.decompose(reps=%d, gates_to_decompose=%s)\n\n"
            gate_str
            (string_of_list string_of_int gate_l)
            (StringSet.cardinal gatenames)
            (string_of_list
               (fun x -> "\"" ^ x ^ "\"")
               (List.of_seq (StringSet.to_seq gatenames)))
        ^ footer

let () =
  let prog_filename = Sys.argv.(1) in
  let out_filename = Sys.argv.(2) in
  let prog_string = read_file prog_filename in
  let qunity_stdlib = read_file "bin/stdlib.qunity" in
  let s = qunity_stdlib ^ "\n" ^ prog_string in
    match
      try Some (parse_file s) with
      | _ -> None
    with
    | None -> Printf.printf "Parse error\n\n"
    | Some qf -> begin
        match preprocess qf with
        | NoneE err -> Printf.printf "Preprocessing error: %s\n\n" err
        | SomeE e -> begin
            let gate, nqubits, out_reg = expr_compile e in
            let qiskit_str = unitary_to_qiskit_file gate nqubits out_reg in
            let out_file = open_out out_filename in
              Printf.fprintf out_file "%s" qiskit_str
          end
      end
