open Util
open Reals
open Syntax
open Typechecking

let debug_mode = false

type gate =
  | Identity
  | PauliX of int
  | Had of int
  | U3Gate of (int * real * real * real)
  | GphaseGate of int list * real
  | Reset of int
  | Swap of int * int
  | Controlled of (int list * bool list * gate)
  | Sequence of (gate * gate)

let ( @& ) a b = Sequence (a, b)

type circuit = {
  in_regs : int list list;
  prep_reg : int list;
  out_regs : int list list;
  flag_reg : int list;
  garb_reg : int list;
  used_wires : IntSet.t;
  gate : gate;
}

type circuit_spec = {
  in_sizes : int list;
  out_sizes : int list;
  (* in_regs, used_wires, reset_garb *)
  circ_fun : int list list -> IntSet.t -> bool -> circuit;
}

type inter_valuation = int list StringMap.t

type inter_op =
  | IEmpty
  | IIdentity of exprtype (* a {T} -> a *)
  | IU3 of real * real * real
  | IGphase of exprtype * real
  | ILeft of exprtype * exprtype (* a {T0} -> left{T0, T1} a *)
  | IRight of exprtype * exprtype (* a {T1} -> right{T0, T1} a *)
  | IPair of exprtype * exprtype (* a {T0}, b {T1} -> (a, b) {T0 * T1} *)
  | IShare of exprtype (* a {T} -> [a; a] *)
  | IContextShare of context
  | IAdjoint of inter_op
  | IDirsum of inter_op * inter_op
  | IAssoc of
      exprtype * exprtype * exprtype (* (T0 + T1) + T2 -> T0 + (T1 + T2) *)
  | IDistrLeft of
      exprtype * exprtype * exprtype (* T * (T0 + T1) -> T * T0 + T * T1 *)
  | IDistrRight of
      exprtype * exprtype * exprtype (* (T0 + T1) * T -> T0 * T + T1 * T *)
  | IMixedErr of inter_op
  | IPureErr of inter_op
  | IDiscard of exprtype
  | IContextDiscard of context
  | IPurify of inter_op
  | IContextPartition of
      context * StringSet.t (* context -> [things in set; things not in set] *)
  | IContextMerge of (context * context)
    (* merge contexts assumed to be disjoint *)
  | ILambda of ((string * int) list * inter_com list * (string * int) list)
(* Output sizes, arg names with sizes, body, return vars. *)

and inter_com = string list * inter_op * string list

let inter_lambda (arglist : (string * int) list) (body : inter_com list)
    (ret : (string * int) list) : inter_op =
  ILambda (arglist, body, ret)

let inter_letapp (target : string list) (op : inter_op) (args : string list) =
  (target, op, args)

let build_circuit (cs : circuit_spec) (in_regs : int list list)
    (used_wires : IntSet.t) : circuit =
  cs.circ_fun in_regs used_wires true

let rec gate_adjoint (u : gate) : gate =
  match u with
  | Identity -> Identity
  | PauliX i -> PauliX i
  | Had i -> Had i
  | U3Gate (i, theta, phi, lambda) ->
      U3Gate (i, Negate theta, Negate lambda, Negate phi)
  | GphaseGate (l, theta) -> GphaseGate (l, Negate theta)
  | Reset _ -> Identity
  | Swap (i, j) -> Swap (i, j)
  | Controlled (l, bl, u0) -> Controlled (l, bl, gate_adjoint u0)
  | Sequence (u0, u1) -> Sequence (gate_adjoint u1, gate_adjoint u0)

(*
Rewire a gate by replacing all references to certain qubits with
other qubits.
*)
let rec gate_rewire (u : gate) (rewiring : int IntMap.t) : gate =
  match u with
  | Identity -> Identity
  | PauliX i -> PauliX (int_map_find_or_keep i rewiring)
  | Had i -> Had (int_map_find_or_keep i rewiring)
  | U3Gate (i, theta, phi, lambda) ->
      U3Gate (int_map_find_or_keep i rewiring, theta, phi, lambda)
  | GphaseGate (l, theta) ->
      GphaseGate (List.map (fun i -> int_map_find_or_keep i rewiring) l, theta)
  | Reset i -> Reset (int_map_find_or_keep i rewiring)
  | Controlled (l, bl, u0) ->
      Controlled
        ( List.map (fun i -> int_map_find_or_keep i rewiring) l,
          bl,
          gate_rewire u0 rewiring )
  | Swap (i, j) ->
      Swap (int_map_find_or_keep i rewiring, int_map_find_or_keep j rewiring)
  | Sequence (u0, u1) ->
      Sequence (gate_rewire u0 rewiring, gate_rewire u1 rewiring)

let gate_cnot (b0 : int) (b1 : int) : gate =
  Controlled ([b0], [true], PauliX b1)

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

let rec gate_distribute_controls (u : gate) : gate =
  match u with
  | Controlled (l, bl, Sequence (u0, u1)) ->
      gate_distribute_controls (Sequence
        ( Controlled (l, bl, gate_distribute_controls u0),
          Controlled (l, bl, gate_distribute_controls u1) ))
  | Controlled (l, bl, Controlled (l', bl', u0)) ->
      Controlled (l @ l', bl @ bl', gate_distribute_controls u0)
  | Sequence (u0, u1) ->
      Sequence (gate_distribute_controls u0, gate_distribute_controls u1)
  | _ -> u

let rec gate_get_max_qubit_index (u : gate) : int =
  match u with
  | Identity -> -1
  | PauliX i -> i
  | Had i -> i
  | U3Gate (i, _, _, _) -> i
  | GphaseGate (l, _) -> int_list_max l
  | Reset i -> i
  | Swap (i, j) -> max i j
  | Controlled (l, _, u0) -> max (int_list_max l) (gate_get_max_qubit_index u0)
  | Sequence (u0, u1) ->
      max (gate_get_max_qubit_index u0) (gate_get_max_qubit_index u1)

let circ_get_total_num_qubits (circ : circuit) : int =
  1
  + max
      (gate_get_max_qubit_index circ.gate)
      (int_list_max (IntSet.elements circ.used_wires))

(*
Share a register to another one by applying CNOT gates for each qubit.
*)
let rec gate_share (reg0 : int list) (reg1 : int list) : gate =
  match (reg0, reg1) with
  | [], [] -> Identity
  | h1 :: t1, h2 :: t2 -> gate_cnot h1 h2 @& gate_share t1 t2
  | _ -> invalid_arg "Registers must be of the same size."

(*
Swap two registers of equal size
*)
let rec gate_swap_regs (reg0 : int list) (reg1 : int list) : gate =
  match (reg0, reg1) with
  | [], [] -> Identity
  | h1 :: t1, h2 :: t2 -> Swap (h1, h2) @& gate_swap_regs t1 t2
  | _ -> invalid_arg "Registers must be of the same size."

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
          Swap (a, b)
          @& gate_permute_helper
               (IntMap.add a (IntMap.find b perm_map) perm_map)
      end
  in
  let perm_map = IntMap.of_seq (List.to_seq (List.combine reg perm)) in
    gate_permute_helper perm_map

let rec gate_lshift (reg : int list) : gate =
  match reg with
  | [] -> Identity
  | [_] -> Identity
  | h1 :: h2 :: t -> Swap (h1, h2) @& gate_lshift (h2 :: t)

let gate_rshift (reg : int list) : gate = gate_adjoint (gate_lshift reg)

let rec gate_reset_reg (reg : int list) : gate =
  match reg with
  | [] -> Identity
  | h :: t -> Reset h @& gate_reset_reg t

let controlled_circ (l : int list) (bl : bool list) (circ : circuit) : gate =
  let circ_in = List.concat circ.in_regs @ circ.prep_reg in
  let circ_out = List.concat circ.out_regs @ circ.flag_reg @ circ.garb_reg in
    Controlled (l, bl, circ.gate)
    @& gate_permute circ_out circ_in
    @& Controlled (l, bl, gate_permute circ_in circ_out)

(*
Number of qubits needed to represent a value of a given size
*)
let rec type_size (t : exprtype) =
  match t with
  | Void
  | Qunit ->
      0
  | SumType (t0, t1) -> 1 + max (type_size t0) (type_size t1)
  | ProdType (t0, t1) -> type_size t0 + type_size t1

let context_size (d : context) =
  List.fold_left ( + ) 0
    (List.map (fun (_, t) -> type_size t) (StringMap.bindings d))

let expect_single_in_reg (name : string) (in_regs : int list list) (size : int)
    : int list =
  match in_regs with
  | [in_reg] when List.length in_reg = size -> in_reg
  | _ ->
      failwith
        (Printf.sprintf "%s: expected input lengths [%d], got %s" name size
           (string_of_list string_of_int (List.map List.length in_regs)))

let circuit_empty =
  {
    in_sizes = [];
    out_sizes = [0];
    circ_fun =
      begin
        fun in_regs used_wires _ ->
          assert (in_regs = []);
          {
            in_regs;
            prep_reg = [];
            out_regs = [[]];
            flag_reg = [];
            garb_reg = [];
            used_wires;
            gate = Identity;
          }
      end;
  }

let circuit_identity (t : exprtype) : circuit_spec =
  {
    in_sizes = [type_size t];
    out_sizes = [type_size t];
    circ_fun =
      begin
        fun in_regs used_wires _ ->
          {
            in_regs;
            prep_reg = [];
            out_regs = in_regs;
            flag_reg = [];
            garb_reg = [];
            used_wires;
            gate = Identity;
          }
      end;
  }

let circuit_u3 (theta : real) (phi : real) (lambda : real) : circuit_spec =
  {
    in_sizes = [1];
    out_sizes = [1];
    circ_fun =
      begin
        fun in_regs used_wires _ ->
          assert (List.map List.length in_regs = [1]);
          let bit = List.hd (List.hd in_regs) in
            {
              in_regs;
              prep_reg = [];
              out_regs = in_regs;
              flag_reg = [];
              garb_reg = [];
              used_wires;
              gate =
                begin
                  let e = U3 (theta, phi, lambda) in
                    if e = qnot then
                      PauliX bit
                    else if e = had then
                      Had bit
                    else
                      U3Gate (bit, theta, phi, lambda)
                end;
            }
      end;
  }

let circuit_gphase (t : exprtype) (theta : real) : circuit_spec =
  let size = type_size t in
    {
      in_sizes = [size];
      out_sizes = [size];
      circ_fun =
        begin
          fun in_regs used_wires _ ->
            assert (List.map List.length in_regs = [size]);
            {
              in_regs;
              prep_reg = [];
              out_regs = in_regs;
              flag_reg = [];
              garb_reg = [];
              used_wires;
              gate = GphaseGate (List.flatten in_regs, theta);
            }
        end;
    }

let circuit_left (t0 : exprtype) (t1 : exprtype) : circuit_spec =
  let in_size = type_size t0 in
  let out_size = type_size (SumType (t0, t1)) in
  let prep_size = out_size - in_size in
    {
      in_sizes = [in_size];
      out_sizes = [out_size];
      circ_fun =
        begin
          fun in_regs used_wires _ ->
            let in_reg = expect_single_in_reg "circuit_left" in_regs in_size in
            let prep, used_wires = fresh_int_list used_wires prep_size in
              {
                in_regs;
                prep_reg = prep;
                out_regs = [(List.hd prep :: in_reg) @ List.tl prep];
                flag_reg = [];
                garb_reg = [];
                used_wires;
                gate = Identity;
              }
        end;
    }

let circuit_right (t0 : exprtype) (t1 : exprtype) : circuit_spec =
  let in_size = type_size t1 in
  let out_size = type_size (SumType (t0, t1)) in
  let prep_size = out_size - in_size in
    {
      in_sizes = [in_size];
      out_sizes = [out_size];
      circ_fun =
        begin
          fun in_regs used_wires _ ->
            let in_reg =
              expect_single_in_reg "circuit_right" in_regs in_size
            in
            let prep, used_wires = fresh_int_list used_wires prep_size in
              {
                in_regs;
                prep_reg = prep;
                out_regs = [(List.hd prep :: in_reg) @ List.tl prep];
                flag_reg = [];
                garb_reg = [];
                used_wires;
                gate = PauliX (List.hd prep);
              }
        end;
    }

let circuit_share (size : int) : circuit_spec =
  {
    in_sizes = [size];
    out_sizes = [size; size];
    circ_fun =
      begin
        fun in_regs used_wires _ ->
          let in_reg = expect_single_in_reg "circuit_share" in_regs size in
          let prep, used_wires = fresh_int_list used_wires size in
            {
              in_regs;
              prep_reg = prep;
              out_regs = [in_reg; prep];
              flag_reg = [];
              garb_reg = [];
              used_wires;
              gate = gate_share in_reg prep;
            }
      end;
  }

let circuit_pair (t0 : exprtype) (t1 : exprtype) : circuit_spec =
  {
    in_sizes = [type_size t0; type_size t1];
    out_sizes = [type_size t0 + type_size t1];
    circ_fun =
      begin
        fun in_regs used_wires _ ->
          let in_reg0, in_reg1 =
            match in_regs with
            | [in_reg0; in_reg1]
              when List.length in_reg0 = type_size t0
                   && List.length in_reg1 = type_size t1 ->
                (in_reg0, in_reg1)
            | _ -> failwith "Invalid input regs"
          in
            {
              in_regs;
              prep_reg = [];
              out_regs = [in_reg0 @ in_reg1];
              flag_reg = [];
              garb_reg = [];
              used_wires;
              gate = Identity;
            }
      end;
  }

let circuit_adjoint (cs : circuit_spec) : circuit_spec =
  {
    in_sizes = cs.out_sizes;
    out_sizes = cs.in_sizes;
    circ_fun =
      begin
        fun in_regs used_wires reset_garb ->
          assert (List.map List.length in_regs = cs.out_sizes);
          let temp_regs, temp_used_wires =
            fresh_int_lists used_wires cs.in_sizes
          in
          let temp_used_wires = IntSet.union used_wires temp_used_wires in
          let circ = cs.circ_fun temp_regs temp_used_wires reset_garb in
            if circ.garb_reg <> [] then
              failwith
                "Expected garbage register to be empty when taking adjoint";
            let rewiring =
              IntMap.of_seq
                (List.to_seq
                   (List.combine
                      (List.flatten circ.out_regs)
                      (List.flatten in_regs)))
            in
            let used_wires =
              IntSet.diff circ.used_wires
                (IntSet.of_list (List.flatten circ.out_regs))
            in
            let rewire_list =
              List.map (fun i -> int_map_find_or_keep i rewiring)
            in
              {
                in_regs;
                prep_reg = rewire_list circ.flag_reg;
                out_regs = List.map rewire_list temp_regs;
                flag_reg = rewire_list circ.prep_reg;
                garb_reg = [];
                used_wires;
                gate = gate_adjoint (gate_rewire circ.gate rewiring);
              }
      end;
  }

let circuit_dirsum (cs0 : circuit_spec) (cs1 : circuit_spec) : circuit_spec =
  let in_size0, in_size1, out_size0, out_size1 =
    match (cs0.in_sizes, cs1.in_sizes, cs0.out_sizes, cs1.out_sizes) with
    | [in_size0], [in_size1], [out_size0], [out_size1] ->
        (in_size0, in_size1, out_size0, out_size1)
    | _ -> assert false
  in
  let in_size = 1 + max in_size0 in_size1 in
  let out_size = 1 + max out_size0 out_size1 in
    {
      in_sizes = [in_size];
      out_sizes = [out_size];
      circ_fun =
        begin
          fun in_regs used_wires reset_garb ->
            let in_reg =
              expect_single_in_reg "circuit_dirsum" in_regs in_size
            in
              assert (List.length in_reg = in_size);
              let ctrlbit = List.hd in_reg in
              let in_reg = List.tl in_reg in
              let min_in_size = min in_size0 in_size1 in
              let diff_in_size = max in_size0 in_size1 - min_in_size in
              let in_reg_min, in_reg_diff =
                list_split_at_i in_reg min_in_size
              in
              let prep_min, used_wires = fresh_int_list used_wires in_size0 in
              let prep_diff, used_wires =
                fresh_int_list used_wires diff_in_size
              in
              let circ0, circ1 =
                if in_size0 <= in_size1 then
                  let circ0 =
                    cs0.circ_fun [in_reg_min] used_wires reset_garb
                  in
                  let circ1 =
                    cs1.circ_fun
                      [prep_min @ in_reg_diff]
                      circ0.used_wires reset_garb
                  in
                    (circ0, circ1)
                else
                  let circ0 =
                    cs0.circ_fun [prep_min @ in_reg_diff] used_wires reset_garb
                  in
                  let circ1 =
                    cs1.circ_fun [in_reg_min] circ0.used_wires reset_garb
                  in
                    (circ0, circ1)
              in
                if circ0.garb_reg <> [] || circ1.garb_reg <> [] then
                  failwith
                    "Expected garbage registers to be empty when taking \
                     direct sum";
                let out_reg0, out_reg1 =
                  match (circ0.out_regs, circ1.out_regs) with
                  | [out_reg0], [out_reg1] -> (out_reg0, out_reg1)
                  | _ -> assert false
                in
                let out_reg_min_in_min, out_reg_min_in_max, out_reg_diff =
                  if out_size0 <= out_size1 then
                    let out_reg_min_in_min = out_reg0 in
                    let out_reg_min_in_max, out_reg_diff =
                      list_split_at_i out_reg1 out_size0
                    in
                      (out_reg_min_in_min, out_reg_min_in_max, out_reg_diff)
                  else
                    let out_reg_min_in_min = out_reg1 in
                    let out_reg_min_in_max, out_reg_diff =
                      list_split_at_i out_reg0 out_size1
                    in
                      (out_reg_min_in_min, out_reg_min_in_max, out_reg_diff)
                in
                  {
                    in_regs;
                    prep_reg =
                      prep_min @ circ0.prep_reg @ circ1.prep_reg @ prep_diff;
                    out_regs = [(ctrlbit :: out_reg_min_in_max) @ out_reg_diff];
                    flag_reg =
                      out_reg_min_in_min @ circ0.flag_reg @ circ1.flag_reg
                      @ prep_diff;
                    garb_reg = [];
                    used_wires;
                    gate =
                      Controlled
                        ( [ctrlbit],
                          [not (in_size0 <= in_size1)],
                          gate_swap_regs in_reg_diff prep_diff )
                      @& Controlled
                           ( [ctrlbit],
                             [in_size0 <= in_size1],
                             gate_swap_regs in_reg_min prep_min )
                      @& controlled_circ [ctrlbit] [false] circ0
                      @& controlled_circ [ctrlbit] [true] circ1
                      @& Controlled
                           ( [ctrlbit],
                             [not (out_size0 <= out_size1)],
                             gate_swap_regs out_reg_min_in_min
                               out_reg_min_in_max );
                  }
        end;
    }

let circuit_assoc (t0 : exprtype) (t1 : exprtype) (t2 : exprtype) :
    circuit_spec =
  (* 1 + max(1 + max(s0, s1), s2) *)
  let in_size = type_size (SumType (SumType (t0, t1), t2)) in
  (* 1 + max(s0, 1 + max(s1, s2)) *)
  let out_size = type_size (SumType (t0, SumType (t1, t2))) in
  let total_size =
    3 + max (type_size t0) (max (type_size t1) (type_size t2))
  in
    {
      in_sizes = [in_size];
      out_sizes = [out_size];
      circ_fun =
        begin
          fun in_regs used_wires _ ->
            let in_reg =
              expect_single_in_reg "circuit_assoc" in_regs in_size
            in
            let signal_01v2 = List.hd in_reg in
            let signal_0v1 = List.hd (List.tl in_reg) in
            let prep_reg, used_wires =
              fresh_int_list used_wires (total_size - in_size)
            in
            let out_reg, flag_reg =
              list_split_at_i (in_reg @ prep_reg) out_size
            in
              {
                in_regs;
                prep_reg;
                out_regs = [out_reg];
                flag_reg;
                garb_reg = [];
                used_wires;
                gate =
                  Controlled
                    ( [signal_01v2],
                      [true],
                      gate_rshift (List.tl in_reg @ prep_reg) )
                  @& gate_cnot signal_01v2 signal_0v1
                  @& Swap (signal_01v2, signal_0v1)
                  @& Controlled
                       ( [signal_01v2],
                         [false],
                         gate_lshift (List.tl in_reg @ prep_reg) );
              }
        end;
    }

let circuit_distr_left (t : exprtype) (t0 : exprtype) (t1 : exprtype) :
    circuit_spec =
  let in_sizes = [type_size t; type_size (SumType (t0, t1))] in
  let out_size = type_size (SumType (ProdType (t, t0), ProdType (t, t1))) in
    {
      in_sizes;
      out_sizes = [out_size];
      circ_fun =
        begin
          fun in_regs used_wires _ ->
            let t_reg, rest_reg =
              match in_regs with
              | [t_reg; rest_reg] when List.map List.length in_regs = in_sizes
                ->
                  (t_reg, rest_reg)
              | _ -> failwith "Invalid input regs"
            in
              {
                in_regs;
                prep_reg = [];
                out_regs = [(List.hd rest_reg :: t_reg) @ List.tl rest_reg];
                flag_reg = [];
                garb_reg = [];
                used_wires;
                gate = Identity;
              }
        end;
    }

let circuit_distr_right (t0 : exprtype) (t1 : exprtype) (t : exprtype) :
    circuit_spec =
  let in_sizes = [type_size (SumType (t0, t1)); type_size t] in
  let out_size = type_size (SumType (ProdType (t0, t), ProdType (t1, t))) in
    {
      in_sizes;
      out_sizes = [out_size];
      circ_fun =
        begin
          fun in_regs used_wires _ ->
            assert (List.map List.length in_regs = in_sizes);
            let rest_reg, t_reg =
              match in_regs with
              | [rest_reg; t_reg] when List.map List.length in_regs = in_sizes
                ->
                  (rest_reg, t_reg)
              | _ -> assert false
            in
              {
                in_regs;
                prep_reg = [];
                out_regs = [rest_reg @ t_reg];
                flag_reg = [];
                garb_reg = [];
                used_wires;
                gate = Identity;
              }
        end;
    }

let circuit_mixed_error_handling (cs : circuit_spec) : circuit_spec =
  let in_sizes = cs.in_sizes in
  let out_size = 1 + List.fold_left ( + ) 0 cs.out_sizes in
    {
      in_sizes;
      out_sizes = [out_size];
      circ_fun =
        begin
          fun in_regs used_wires reset_garb ->
            assert (List.map List.length in_regs = in_sizes);
            let circ = cs.circ_fun in_regs used_wires reset_garb in
            let used_wires = circ.used_wires in
            let new_prep, used_wires = fresh_int_list used_wires out_size in
            let signal_bit = List.hd new_prep in
            let fresh_prep = List.tl new_prep in
            let garb_reg = circ.flag_reg @ circ.garb_reg @ fresh_prep in
              {
                in_regs;
                prep_reg = (signal_bit :: circ.prep_reg) @ fresh_prep;
                out_regs = [signal_bit :: List.concat circ.out_regs];
                flag_reg = [];
                garb_reg;
                used_wires =
                  begin
                    if reset_garb then
                      IntSet.diff used_wires (IntSet.of_list garb_reg)
                    else
                      used_wires
                  end;
                gate =
                  circ.gate @& PauliX signal_bit
                  @& begin
                       if circ.flag_reg = [] then
                         Identity
                       else
                         Controlled
                           ( circ.flag_reg,
                             list_constant false (List.length circ.flag_reg),
                             PauliX signal_bit
                             @& gate_swap_regs
                                  (List.concat circ.out_regs)
                                  fresh_prep )
                     end
                  @& begin
                       if reset_garb then
                         gate_reset_reg garb_reg
                       else
                         Identity
                     end;
              }
        end;
    }

let circuit_pure_error_handling (cs : circuit_spec) : circuit_spec =
  let in_sizes = cs.in_sizes in
  let out_size = 1 + List.fold_left ( + ) 0 cs.out_sizes in
    {
      in_sizes;
      out_sizes = [out_size];
      circ_fun =
        begin
          fun in_regs used_wires reset_garb ->
            assert (List.map List.length in_regs = in_sizes);
            let circ = cs.circ_fun in_regs used_wires reset_garb in
              if circ.garb_reg <> [] then
                failwith
                  "Expected empty garbage register for pure error handling";
              let used_wires = circ.used_wires in
              let signal_bit, used_wires = fresh_int_list used_wires 1 in
              let signal_bit = List.hd signal_bit in
                {
                  in_regs;
                  prep_reg = signal_bit :: circ.prep_reg;
                  out_regs =
                    [(signal_bit :: List.concat circ.out_regs) @ circ.flag_reg];
                  flag_reg = [];
                  garb_reg = [];
                  used_wires;
                  gate =
                    circ.gate @& PauliX signal_bit
                    @& begin
                         if circ.flag_reg = [] then
                           Identity
                         else
                           Controlled
                             ( circ.flag_reg,
                               list_constant false (List.length circ.flag_reg),
                               PauliX signal_bit )
                       end;
                }
        end;
    }

let circuit_discard (size : int) =
  {
    in_sizes = [size];
    out_sizes = [];
    circ_fun =
      begin
        fun in_regs used_wires reset_garb ->
          let garb_reg = List.concat in_regs in
            {
              in_regs;
              prep_reg = [];
              out_regs = [];
              flag_reg = [];
              garb_reg;
              used_wires =
                begin
                  if reset_garb then
                    IntSet.diff used_wires (IntSet.of_list garb_reg)
                  else
                    used_wires
                end;
              gate =
                begin
                  if reset_garb then gate_reset_reg garb_reg else Identity
                end;
            }
      end;
  }

let circuit_purify (cs : circuit_spec) : circuit_spec =
  {
    in_sizes = cs.in_sizes;
    out_sizes =
      begin
        let temp_circ =
          cs.circ_fun
            (List.map (list_constant (-1)) cs.in_sizes)
            IntSet.empty true
        in
          List.map List.length temp_circ.out_regs
      end;
    circ_fun =
      begin
        fun in_regs used_wires _ -> cs.circ_fun in_regs used_wires false
      end;
  }

let context_reg_split (d : context) (reg : int list) : int list StringMap.t =
  let rec iter (cur : int list StringMap.t) (reg_rest : int list)
      (bind_rest : (string * exprtype) list) : int list StringMap.t =
    match bind_rest with
    | [] -> cur
    | (x, t) :: tl -> begin
        let newreg, newrest = list_split_at_i reg_rest (type_size t) in
          iter (StringMap.add x newreg cur) newrest tl
      end
  in
    iter StringMap.empty reg (StringMap.bindings d)

let context_reg_partition (d : context) (reg : int list) (fv : StringSet.t) :
    int list * int list =
  let reg_map = context_reg_split d reg in
  let reg_map_in = StringMap.filter (fun x _ -> StringSet.mem x fv) reg_map in
  let reg_map_out =
    StringMap.filter (fun x _ -> not (StringSet.mem x fv)) reg_map
  in
    assert (
      StringSet.inter (map_dom reg_map_in) (map_dom reg_map_out)
      = StringSet.empty);
    let flatten_bindings = List.fold_left (fun l (_, l') -> l @ l') [] in
      ( flatten_bindings (StringMap.bindings reg_map_in),
        flatten_bindings (StringMap.bindings reg_map_out) )

let circuit_context_partition (d : context) (fv : StringSet.t) : circuit_spec =
  let in_size = context_size d in
  let d0, d1 = map_partition d fv in
    {
      in_sizes = [in_size];
      out_sizes = [StringMap.cardinal d0; StringMap.cardinal d1];
      circ_fun =
        begin
          fun in_regs used_wires _ ->
            let in_reg =
              expect_single_in_reg "circuit_context_partition" in_regs in_size
            in
            let out0, out1 = context_reg_partition d in_reg fv in
              {
                in_regs;
                prep_reg = [];
                out_regs = [out0; out1];
                flag_reg = [];
                garb_reg = [];
                used_wires;
                gate = Identity;
              }
        end;
    }

let circuit_context_merge (d0 : context) (d1 : context) : circuit_spec =
  let d =
    match map_merge false d0 d1 with
    | SomeE d -> d
    | NoneE err -> failwith err
  in
  let fv0 = map_dom d0 in
    circuit_adjoint (circuit_context_partition d fv0)

let rec compile_inter_com_to_circuit (circ : circuit) (iv : inter_valuation)
    (ie : inter_com) : circuit * inter_valuation =
  let used_wires = circ.used_wires in
  let target, op, args = ie in
  let argset = StringSet.of_list args in
    if
      StringSet.inter (StringSet.of_list target)
        (StringSet.diff (map_dom iv) argset)
      <> StringSet.empty
    then
      failwith "Attempted to store new value in existing variable"
    else
      let ev_regs =
        List.map
          (fun x ->
            match StringMap.find_opt x iv with
            | Some reg -> reg
            | None -> failwith (Printf.sprintf "Variable %s not found" x))
          args
      in
      let op_cs = compile_inter_op_to_circuit op in
      let op_circ = build_circuit op_cs ev_regs used_wires in
      let used_wires = op_circ.used_wires in
      let res_circ =
        {
          in_regs = circ.in_regs;
          prep_reg = circ.prep_reg @ op_circ.prep_reg;
          out_regs = op_circ.out_regs;
          flag_reg = circ.flag_reg @ op_circ.flag_reg;
          garb_reg = circ.garb_reg @ op_circ.garb_reg;
          used_wires;
          gate = circ.gate @& op_circ.gate;
        }
      in
        if debug_mode then
          Printf.printf
            "Apply: target = %s, args = %s, out_regs = %s, ev_regs = %s\n"
            (string_of_list (fun s -> s) target)
            (string_of_list (fun s -> s) args)
            (string_of_list (string_of_list string_of_int) op_circ.out_regs)
            (string_of_list (string_of_list string_of_int) ev_regs);
        let iv' =
          StringMap.filter (fun x _ -> not (StringSet.mem x argset)) iv
        in
        let iv_add = List.combine target op_circ.out_regs in
        let iv'' =
          match
            map_merge false iv' (StringMap.of_seq (List.to_seq iv_add))
          with
          | SomeE iv'' -> iv''
          | NoneE err -> failwith err
        in
          (res_circ, iv'')

and compile_inter_com_list_to_circuit (circ : circuit) (iv : inter_valuation)
    (iel : inter_com list) : circuit * inter_valuation =
  match iel with
  | [] -> (circ, iv)
  | ielh :: ielt -> begin
      let circ', iv' = compile_inter_com_to_circuit circ iv ielh in
        compile_inter_com_list_to_circuit circ' iv' ielt
    end

and compile_inter_op_to_circuit (op : inter_op) : circuit_spec =
  match op with
  | IEmpty -> circuit_empty
  | IIdentity t -> circuit_identity t
  | IU3 (theta, phi, lambda) -> circuit_u3 theta phi lambda
  | IGphase (t, theta) -> circuit_gphase t theta
  | ILeft (t0, t1) -> circuit_left t0 t1
  | IRight (t0, t1) -> circuit_right t0 t1
  | IPair (t0, t1) -> circuit_pair t0 t1
  | IShare t -> circuit_share (type_size t)
  | IContextShare d -> circuit_share (context_size d)
  | IAdjoint op' -> circuit_adjoint (compile_inter_op_to_circuit op')
  | IDirsum (op0, op1) ->
      circuit_dirsum
        (compile_inter_op_to_circuit op0)
        (compile_inter_op_to_circuit op1)
  | IAssoc (t0, t1, t2) -> circuit_assoc t0 t1 t2
  | IDistrLeft (t, t0, t1) -> circuit_distr_left t t0 t1
  | IDistrRight (t0, t1, t) -> circuit_distr_right t0 t1 t
  | IMixedErr op ->
      circuit_mixed_error_handling (compile_inter_op_to_circuit op)
  | IPureErr op -> circuit_pure_error_handling (compile_inter_op_to_circuit op)
  | IDiscard t -> circuit_discard (type_size t)
  | IContextDiscard d -> circuit_discard (context_size d)
  | IPurify op' -> circuit_purify (compile_inter_op_to_circuit op')
  | IContextPartition (d, fv) -> circuit_context_partition d fv
  | IContextMerge (d0, d1) -> circuit_context_merge d0 d1
  | ILambda (args, iel, ret) -> begin
      let arg_names = List.map fst args in
      let arg_sizes = List.map snd args in
      let ret_names = List.map fst ret in
      let ret_sizes = List.map snd ret in
        {
          in_sizes = arg_sizes;
          out_sizes = ret_sizes;
          circ_fun =
            begin
              fun in_regs used_wires _ ->
                if debug_mode then
                  Printf.printf
                    "ILambda: arg_names = %s, in_regs = %s, arg_sizes = %s, \
                     ret_sizes = %s\n"
                    (string_of_list (fun s -> s) arg_names)
                    (string_of_list (string_of_list string_of_int) in_regs)
                    (string_of_list string_of_int arg_sizes)
                    (string_of_list string_of_int ret_sizes);
                let iv =
                  StringMap.of_seq
                    (List.to_seq (List.combine arg_names in_regs))
                in
                let init_circ =
                  {
                    in_regs;
                    prep_reg = [];
                    out_regs = in_regs;
                    flag_reg = [];
                    garb_reg = [];
                    used_wires;
                    gate = Identity;
                  }
                in
                let circ, iv =
                  compile_inter_com_list_to_circuit init_circ iv iel
                in
                let ret_regs =
                  List.map (fun x -> StringMap.find x iv) ret_names
                in
                  {
                    in_regs;
                    prep_reg = circ.prep_reg;
                    out_regs = ret_regs;
                    flag_reg = circ.flag_reg;
                    garb_reg = circ.garb_reg;
                    used_wires = circ.used_wires;
                    gate = circ.gate;
                  }
            end;
        }
    end

let rec compile_pure_expr_to_inter_op (g : context) (d : context) (e : expr) :
    inter_op =
  let t =
    match pure_type_check g d e with
    | SomeE t -> t
    | NoneE err -> failwith err
  in
  let gsize = context_size g in
  let dsize = context_size d in
  let tsize = type_size t in
    match e with
    | Null -> begin
        inter_lambda
          [("g", gsize); ("d", dsize)]
          []
          [("g", gsize); ("d", tsize)]
      end
    | Var x -> begin
        match StringMap.bindings d with
        | [] -> begin
            let xreg, grest = map_partition g (StringSet.singleton x) in
              inter_lambda
                [("g", gsize); ("d", dsize)]
                [
                  inter_letapp ["x"; "rest"]
                    (IContextPartition (g, StringSet.singleton x))
                    ["g"];
                  inter_letapp ["x"; "res"] (IShare t) ["x"];
                  inter_letapp ["g"]
                    (IContextMerge (xreg, grest))
                    ["x"; "rest"];
                ]
                [("g", gsize); ("res", tsize)]
          end
        | [(x', _)] when x' = x -> begin
            inter_lambda
              [("g", gsize); ("d", dsize)]
              []
              [("g", gsize); ("d", tsize)]
          end
        | _ ->
            failwith
              "Variable not found or irrelevant variables in quantum context"
      end
    | Qpair (e0, e1) -> begin
        let t0, t1 =
          match t with
          | ProdType (t0, t1) -> (t0, t1)
          | _ -> failwith "Expected product type"
        in
        let fv0 = free_vars e0 in
        let fv1 = free_vars e1 in
        let fv01 = StringSet.inter fv0 fv1 in
        let d01, d_xor = map_partition d fv01 in
        let d0 = map_restriction d fv0 in
        let d1 = map_restriction d fv1 in
        let d0' = map_restriction d_xor fv0 in
        let d1' = map_restriction d_xor fv1 in
        let op0 = compile_pure_expr_to_inter_op g d0 e0 in
        let op1 = compile_pure_expr_to_inter_op g d1 e1 in
          inter_lambda
            [("g", gsize); ("d", dsize)]
            [
              inter_letapp ["d01"; "d_xor"] (IContextPartition (d, fv01)) ["d"];
              inter_letapp ["d0*"; "d1*"]
                (IContextPartition (d_xor, fv0))
                ["d_xor"];
              inter_letapp ["d01"; "d01*"] (IContextShare d01) ["d01"];
              inter_letapp ["d01_0"] (IContextMerge (d01, d0')) ["d01"; "d0*"];
              inter_letapp ["d01*_1"]
                (IContextMerge (d01, d1'))
                ["d01*"; "d1*"];
              inter_letapp ["g"; "t0"] op0 ["g"; "d01_0"];
              inter_letapp ["g"; "t1"] op1 ["g"; "d01*_1"];
              inter_letapp ["res"] (IPair (t0, t1)) ["t0"; "t1"];
            ]
            [("g", gsize); ("res", tsize)]
      end
    | Ctrl _ -> failwith "TODO"
    | Try _ -> failwith "Try is not a pure expression"
    | Apply (f, e') -> begin
        let e'_op = compile_pure_expr_to_inter_op g d e' in
        let f_op = compile_pure_prog_to_inter_op f in
          inter_lambda
            [("g", gsize); ("d", dsize)]
            [
              inter_letapp ["g"; "t*"] e'_op ["g"; "d"];
              inter_letapp ["res"] f_op ["t*"];
            ]
            [("g", gsize); ("res", tsize)]
      end

and compile_mixed_expr_to_inter_op (d : context) (e : expr) : inter_op =
  let t =
    match mixed_type_check d e with
    | SomeE t -> t
    | NoneE err -> failwith err
  in
  let dsize = context_size d in
  let tsize = type_size t in
    match pure_type_check StringMap.empty d e with
    | SomeE _ -> begin
        let e_op = compile_pure_expr_to_inter_op StringMap.empty d e in
          inter_lambda
            [("d", dsize)]
            [
              inter_letapp ["g"] IEmpty [];
              inter_letapp ["g"; "res"] e_op ["g"; "d"];
              inter_letapp [] (IAdjoint IEmpty) ["g"];
            ]
            [("res", tsize)]
      end
    | _ -> begin
        match e with
        | Qpair (e0, e1) -> begin
            let t0, t1 =
              match t with
              | ProdType (t0, t1) -> (t0, t1)
              | _ -> failwith "Expected product type"
            in
            let fv0 = free_vars e0 in
            let fv1 = free_vars e1 in
            let fv01 = StringSet.inter fv0 fv1 in
            let d01, d_xor = map_partition d fv01 in
            let d0 = map_restriction d fv0 in
            let d1 = map_restriction d fv1 in
            let d0' = map_restriction d_xor fv0 in
            let d1' = map_restriction d_xor fv1 in
            let op0 = compile_mixed_expr_to_inter_op d0 e0 in
            let op1 = compile_mixed_expr_to_inter_op d1 e1 in
              inter_lambda
                [("d", dsize)]
                [
                  inter_letapp ["d01"; "d_xor"]
                    (IContextPartition (d, fv01))
                    ["d"];
                  inter_letapp ["d0*"; "d1*"]
                    (IContextPartition (d_xor, fv0))
                    ["d_xor"];
                  inter_letapp ["d01"; "d01*"] (IContextShare d01) ["d01"];
                  inter_letapp ["d01_0"]
                    (IContextMerge (d01, d0'))
                    ["d01"; "d0*"];
                  inter_letapp ["d01*_1"]
                    (IContextMerge (d01, d1'))
                    ["d01*"; "d1*"];
                  inter_letapp ["t0"] op0 ["d01_0"];
                  inter_letapp ["t1"] op1 ["d01*_1"];
                  inter_letapp ["res"] (IPair (t0, t1)) ["t0"; "t1"];
                ]
                [("res", tsize)]
          end
        | Try (e0, e1) -> begin
            let fv0 = free_vars e0 in
            let fv1 = free_vars e1 in
            let d0 = map_restriction d fv0 in
            let d1 = map_restriction d fv1 in
            let op0 = compile_mixed_expr_to_inter_op d0 e0 in
            let op1 = compile_mixed_expr_to_inter_op d1 e1 in
              inter_lambda
                [("d", dsize)]
                [
                  inter_letapp ["d0"; "d1"] (IContextPartition (d, fv0)) ["d"];
                  inter_letapp ["t+c0"] (IMixedErr op0) ["d0"];
                  inter_letapp ["t+c1"] (IMixedErr op1) ["d1"];
                  inter_letapp ["t*(t+c)+(t+c)"]
                    (IDistrRight (t, Qunit, SumType (t, Qunit)))
                    ["t+c0"; "t+c1"];
                  inter_letapp ["(t*(t+c)+t)+c"]
                    (IAdjoint
                       (IAssoc (ProdType (t, SumType (t, Qunit)), t, Qunit)))
                    ["t*(t+c)+(t+c)"];
                  inter_letapp ["t*(t+c)+t"]
                    (IAdjoint
                       (ILeft
                          (SumType (ProdType (t, SumType (t, Qunit)), t), Qunit)))
                    ["(t*(t+c)+t)+c"];
                  inter_letapp ["t"; "(t+c)+c"]
                    (IAdjoint (IDistrLeft (t, SumType (t, Qunit), Qunit)))
                    ["t*(t+c)+t"];
                  inter_letapp []
                    (IDiscard (SumType (SumType (t, Qunit), Qunit)))
                    ["(t+c)+c"];
                ]
                [("t", tsize)]
          end
        | Apply (f, e') -> begin
            let e'_op = compile_mixed_expr_to_inter_op d e' in
            let f_op = compile_mixed_prog_to_inter_op f in
              inter_lambda
                [("d", dsize)]
                [
                  inter_letapp ["t*"] e'_op ["d"];
                  inter_letapp ["res"] f_op ["t*"];
                ]
                [("res", tsize)]
          end
        | _ -> failwith "Error in mixed expression compilation"
      end

and compile_pure_prog_to_inter_op (f : prog) : inter_op =
  match f with
  | U3 (theta, phi, lambda) -> IU3 (theta, phi, lambda)
  | Left (t0, t1) -> ILeft (t0, t1)
  | Right (t0, t1) -> IRight (t0, t1)
  | Lambda (e, t, e') -> begin
      let t' =
        match prog_type_check f with
        | SomeE (Coherent (_, t')) -> t'
        | SomeE (Channel _) ->
            failwith "Attempted pure compilation for mixed program"
        | NoneE err -> failwith err
      in
        match context_check StringMap.empty t e with
        | NoneE err -> failwith err
        | SomeE d -> begin
            let e_op = compile_pure_expr_to_inter_op StringMap.empty d e in
            let e'_op = compile_pure_expr_to_inter_op StringMap.empty d e' in
              inter_lambda
                [("t", type_size t)]
                [
                  inter_letapp ["g"] IEmpty [];
                  inter_letapp ["g"; "d"] (IAdjoint e_op) ["g"; "t"];
                  inter_letapp ["g"; "res"] e'_op ["g"; "d"];
                  inter_letapp [] (IAdjoint IEmpty) ["g"];
                ]
                [("res", type_size t')]
          end
    end
  | Gphase (t, theta) -> IGphase (t, theta)

and compile_mixed_prog_to_inter_op (f : prog) : inter_op =
  match (f, prog_type_check f) with
  | _, NoneE err -> failwith err
  | _, SomeE (Coherent _) -> compile_pure_prog_to_inter_op f
  | Lambda (e, t, e'), SomeE (Channel (_, t')) -> begin
      match context_check StringMap.empty t e with
      | NoneE err -> failwith err
      | SomeE d ->
          let fve' = free_vars e' in
          let d', d0 = map_partition d fve' in
          let e_op = compile_mixed_expr_to_inter_op d e in
          let e'_op = compile_mixed_expr_to_inter_op d' e' in
            inter_lambda
              [("t", type_size t)]
              [
                inter_letapp ["d"] (IAdjoint e_op) ["t"];
                inter_letapp ["d*"; "d0"] (IContextPartition (d, fve')) ["d"];
                inter_letapp ["res"] e'_op ["d*"];
                inter_letapp [] (IContextDiscard d0) ["d0"];
              ]
              [("res", type_size t')]
    end
  | _ -> failwith "Error in mixed program compilation"

let expr_compile (e : expr) : gate * int * int list * int list =
  let op = compile_mixed_expr_to_inter_op StringMap.empty e in
  let cs = compile_inter_op_to_circuit op in
  let circ = build_circuit cs [[]] IntSet.empty in
  let nqubits = circ_get_total_num_qubits circ in
  let gate = gate_distribute_controls (gate_remove_identities circ.gate) in
  let out_reg =
    match circ.out_regs with
    | [out_reg] -> out_reg
    | _ -> failwith "Expected single out reg"
  in
    (gate, nqubits, out_reg, circ.flag_reg)
