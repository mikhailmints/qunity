open Driver_util
open Qunity
open Util
open Reals
open Gate

let simple_gate_to_qasm_str (u : gate) : string * int list =
  match u with
  | Identity -> failwith "Identities should be removed"
  | U3Gate (i, theta, phi, lambda) ->
      if
        gate_equal u (gate_paulix i)
        || gate_equal u (gate_adjoint (gate_paulix i))
      then
        ("x", [i])
      else if
        gate_equal u (gate_had i) || gate_equal u (gate_adjoint (gate_had i))
      then
        ("h", [i])
      else
        ( Printf.sprintf "U(%s, %s, %s)"
            (qasm_string_of_real_temp theta)
            (qasm_string_of_real_temp phi)
            (qasm_string_of_real_temp lambda),
          [i] )
  | GphaseGate theta ->
      (Printf.sprintf "gphase(%s)" (qasm_string_of_real_temp theta), [])
  | Reset i -> ("reset", [i])
  | Swap (i, j) -> ("swap", [i; j])
  | _ -> failwith "Expected simple gate"

let rec gate_to_qasm_str (u : gate) : string =
  let arglist_fold cur x =
    if cur = "" then
      Printf.sprintf " q[%d]" x
    else
      cur ^ Printf.sprintf ", q[%d]" x
  in
    match u with
    | Controlled (l, bl, u0) -> begin
        match u0 with
        | Sequence _ -> failwith "Controls should be distributed"
        | Controlled _ -> failwith "Controls should be combined"
        | Annotation (l, s) ->
            if l = [] then
              ""
            else
              Printf.sprintf "// %s %s\n" s (string_of_list string_of_int l)
        | GphaseGate Pi when List.length l = 1 -> Printf.sprintf "z q[%d];\n" (List.hd l)
        | _ ->
            let s, l0 = simple_gate_to_qasm_str u0 in
              List.fold_left
                (fun cur bi ->
                  if bi then cur ^ "ctrl @ " else cur ^ "negctrl @ ")
                "" bl
              ^ s
              ^ List.fold_left arglist_fold "" (l @ l0)
              ^ ";\n"
      end
    | Sequence (u0, u1) -> gate_to_qasm_str u0 ^ gate_to_qasm_str u1
    | Annotation (l, s) ->
        if l = [] then
          ""
        else
          Printf.sprintf "// %s %s\n" s (string_of_list string_of_int l)
    | _ ->
        let s, l = simple_gate_to_qasm_str u in
          Printf.sprintf "%s%s;\n" s (List.fold_left arglist_fold "" l)

let gate_to_qasm_file (u : gate) (nqubits : int) (out_reg : int list)
    (flag_reg : int list) : string =
  let n_out = List.length out_reg in
  let n_flag = List.length flag_reg in
  let header =
    Printf.sprintf
      "OPENQASM 3.0;\n\
       include \"stdgates.inc\";\n\
       qubit[%d] q;\n\
       bit[%d] out;\n\
       bit[%d] err;\n"
      nqubits n_out n_flag
  in
  let body = if u = Identity then "" else gate_to_qasm_str u in
  let footer =
    List.fold_left ( ^ ) ""
      (List.map
         (fun (i, j) -> Printf.sprintf "out[%d] = measure q[%d];\n" i j)
         (List.combine (range n_out) out_reg))
    ^ List.fold_left ( ^ ) ""
        (List.map
           (fun (i, j) -> Printf.sprintf "err[%d] = measure q[%d];\n" i j)
           (List.combine (range n_flag) flag_reg))
  in
    header ^ body ^ footer

let () =
  let prog_filename = Sys.argv.(1) in
  let out_filename = Sys.argv.(2) in
  let annotate = bool_of_string Sys.argv.(3) in
    compile_file prog_filename out_filename gate_to_qasm_file annotate
