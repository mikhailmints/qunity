open Util
open Reals
open Syntax
open Typechecking

type unitary =
  | Identity
  | PauliX of int
  | Had of int
  | U3Gate of (int * real * real * real)
  | GphaseGate of int list * real
  | Controlled of (int list * bool list * unitary)
  | Sequence of (unitary * unitary)

let ( @& ) a b = Sequence (a, b)

type circuit = {
  prep_reg : int list;
  out_regs : int list list;
  flag_reg : int list;
  garb_reg : int list;
  used_wires : IntSet.t;
  gate : unitary;
}

type circuit_spec = {
  in_sizes : int list;
  out_sizes : int list;
  circ_fun : int list list -> IntSet.t -> circuit;
}

type inter_valuation = int list StringMap.t

type inter_op =
  | IIdentity of exprtype (* a {T} -> a *)
  | IU3 of real * real * real
  | IGphase of exprtype * real
  | ILeft of exprtype * exprtype (* a {T0} -> left{T0, T1} a *)
  | IRight of exprtype * exprtype (* a {T1} -> right{T0, T1} a *)
  | IPair of exprtype * exprtype (* a {T0}, b {T1} -> (a, b) {T0 * T1} *)
  | IShare of exprtype (* a {T} -> [a; a] *)
  | IContextShare of context
  | IAdjoint of inter_op
  (* | ISequence of inter_op * inter_op
     | ITensor of inter_op * inter_op
     | IDirsum of inter_op * inter_op
     | IDistr of
         exprtype * exprtype * exprtype (* T * (T0 + T1) -> T * T0 + T * T1 *)
     | IMixedErr of inter_op
     | IPureErr of inter_op
     | IPurify of inter_op *)
  | IContextPartition of
      context * StringSet.t (* context -> [things in set; things not in set] *)
  | IContextMerge of (context * context)
    (* merge contexts assumed to be disjoint *)
  | ILambda of (int * (string * int) list * inter_expr list * string list)
(* Output size, arg names with sizes, body, return vars. *)

and inter_expr = string list * inter_op * string list

let inter_lambda (outsize : int) (arglist : (string * int) list)
    (body : inter_expr list) (ret : string list) : inter_op =
  ILambda (outsize, arglist, body, ret)

let inter_letapp (target : string list) (op : inter_op) (args : string list) =
  (target, op, args)

let rec unitary_adjoint (u : unitary) : unitary =
  match u with
  | Identity -> Identity
  | PauliX i -> PauliX i
  | Had i -> Had i
  | U3Gate (i, theta, phi, lambda) ->
      U3Gate (i, Negate theta, Negate lambda, Negate phi)
  | GphaseGate (l, theta) -> GphaseGate (l, Negate theta)
  | Controlled (l, bl, u0) -> Controlled (l, bl, unitary_adjoint u0)
  | Sequence (u0, u1) -> Sequence (unitary_adjoint u1, unitary_adjoint u0)

(*
Rewire a unitary by replacing all references to certain qubits with
other qubits.
*)
let rec unitary_rewire (u : unitary) (rewiring : int IntMap.t) : unitary =
  match u with
  | Identity -> Identity
  | PauliX i -> PauliX (int_map_find_or_keep i rewiring)
  | Had i -> Had (int_map_find_or_keep i rewiring)
  | U3Gate (i, theta, phi, lambda) ->
      U3Gate (int_map_find_or_keep i rewiring, theta, phi, lambda)
  | GphaseGate (l, theta) ->
      GphaseGate (List.map (fun i -> int_map_find_or_keep i rewiring) l, theta)
  | Controlled (l, bl, u0) ->
      Controlled
        ( List.map (fun i -> int_map_find_or_keep i rewiring) l,
          bl,
          unitary_rewire u0 rewiring )
  | Sequence (u0, u1) ->
      Sequence (unitary_rewire u0 rewiring, unitary_rewire u1 rewiring)

let unitary_cnot (b0 : int) (b1 : int) : unitary =
  Controlled ([b0], [true], PauliX b1)

let rec unitary_remove_identities (u : unitary) : unitary =
  match u with
  | Controlled (l, bl, u0) -> begin
      let u0' = unitary_remove_identities u0 in
        if u0' = Identity then Identity else Controlled (l, bl, u0')
    end
  | Sequence (Identity, Identity) -> Identity
  | Sequence (Identity, u0) -> unitary_remove_identities u0
  | Sequence (u0, Identity) -> unitary_remove_identities u0
  | Sequence (u0, u1) -> begin
      let u0' = unitary_remove_identities u0 in
      let u1' = unitary_remove_identities u1 in
        match (u0', u1') with
        | Identity, _ -> u1'
        | _, Identity -> u0'
        | _, _ -> Sequence (u0', u1')
    end
  | _ -> u

(*
Share a register to another one by applying CNOT gates for each qubit.
*)
let rec unitary_share (reg0 : int list) (reg1 : int list) : unitary =
  match (reg0, reg1) with
  | [], [] -> Identity
  | h1 :: t1, h2 :: t2 -> unitary_cnot h1 h2 @& unitary_share t1 t2
  | _ -> invalid_arg "Registers must be of the same size."

(*
SWAP gate as 3 CNOTs
*)
let unitary_swap (b0 : int) (b1 : int) : unitary =
  unitary_cnot b0 b1 @& unitary_cnot b1 b0 @& unitary_cnot b0 b1

(*
Swap two registers of equal size
*)
let rec unitary_swap_regs (reg0 : int list) (reg1 : int list) : unitary =
  match (reg0, reg1) with
  | [], [] -> Identity
  | h1 :: t1, h2 :: t2 -> unitary_swap h1 h2 @& unitary_swap_regs t1 t2
  | _ -> invalid_arg "Registers must be of the same size."

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

let circuit_identity (t : exprtype) : circuit_spec =
  {
    in_sizes = [type_size t];
    out_sizes = [type_size t];
    circ_fun =
      begin
        fun in_regs used_wires ->
          {
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
        fun in_regs used_wires ->
          assert (List.map List.length in_regs = [1]);
          let bit = List.hd (List.hd in_regs) in
            {
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
          fun in_regs used_wires ->
            assert (List.map List.length in_regs = [size]);
            {
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
          fun in_regs used_wires ->
            let in_reg =
              match in_regs with
              | [in_reg] when List.length in_reg = in_size -> in_reg
              | _ -> failwith "Invalid input regs"
            in
            let prep, used_wires = fresh_int_list used_wires prep_size in
              {
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
          fun in_regs used_wires ->
            let in_reg =
              match in_regs with
              | [in_reg] when List.length in_reg = in_size -> in_reg
              | _ -> failwith "Invalid input regs"
            in
            let prep, used_wires = fresh_int_list used_wires prep_size in
              {
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
        fun in_regs used_wires ->
          let in_reg =
            match in_regs with
            | [in_reg] when List.length in_reg = size -> in_reg
            | _ -> assert false
          in
          let prep, used_wires = fresh_int_list used_wires size in
            {
              prep_reg = prep;
              out_regs = [in_reg; prep];
              flag_reg = [];
              garb_reg = [];
              used_wires;
              gate = unitary_share in_reg prep;
            }
      end;
  }

(* let circuit_indexed_share (sizes : int list) (i : int) : circuit_spec =
   let isize = List.nth sizes i in
     {
       in_sizes = sizes;
       out_sizes = sizes @ [isize];
       circ_fun =
         begin
           fun in_regs used_wires ->
             assert (List.map List.length in_regs = sizes);
             let prep, used_wires = fresh_int_list used_wires isize in
               {
                 prep_reg = prep;
                 out_regs = in_regs @ [prep];
                 flag_reg = [];
                 garb_reg = [];
                 used_wires;
                 gate = unitary_share (List.nth in_regs i) prep;
               }
         end;
     } *)

let circuit_pair (t0 : exprtype) (t1 : exprtype) : circuit_spec =
  {
    in_sizes = [type_size t0; type_size t1];
    out_sizes = [type_size t0 + type_size t1];
    circ_fun =
      begin
        fun in_regs used_wires ->
          let in_reg0, in_reg1 =
            match in_regs with
            | [in_reg0; in_reg1]
              when List.length in_reg0 = type_size t0
                   && List.length in_reg1 = type_size t1 ->
                (in_reg0, in_reg1)
            | _ -> failwith "Invalid input regs"
          in
            {
              prep_reg = [];
              out_regs = [in_reg0 @ in_reg1];
              flag_reg = [];
              garb_reg = [];
              used_wires;
              gate = Identity;
            }
      end;
  }

let circuit_product (cs0 : circuit_spec) (cs1 : circuit_spec) : circuit_spec =
  {
    in_sizes = cs0.in_sizes @ cs1.in_sizes;
    out_sizes = cs0.out_sizes @ cs1.out_sizes;
    circ_fun =
      begin
        fun in_regs used_wires ->
          assert (List.map List.length in_regs = cs0.in_sizes @ cs1.in_sizes);
          let in_regs0, in_regs1 =
            list_split_at_i in_regs (List.length cs0.in_sizes)
          in
          let circ0 = cs0.circ_fun in_regs0 used_wires in
          let circ1 = cs1.circ_fun in_regs1 circ0.used_wires in
          let used_wires = circ1.used_wires in
            {
              prep_reg = circ0.prep_reg @ circ1.prep_reg;
              out_regs = circ0.out_regs @ circ1.out_regs;
              flag_reg = circ0.flag_reg @ circ1.flag_reg;
              garb_reg = circ0.garb_reg @ circ1.garb_reg;
              used_wires;
              gate = circ0.gate @& circ1.gate;
            }
      end;
  }

let ( *& ) = circuit_product

let circuit_sequence (cs0 : circuit_spec) (cs1 : circuit_spec) : circuit_spec =
  {
    in_sizes = cs0.in_sizes;
    out_sizes = cs0.out_sizes;
    circ_fun =
      begin
        fun in_regs used_wires ->
          assert (List.map List.length in_regs = cs0.in_sizes);
          let circ0 = cs0.circ_fun in_regs used_wires in
          let circ1 = cs1.circ_fun circ0.out_regs circ0.used_wires in
          let used_wires = circ1.used_wires in
            {
              prep_reg = circ0.prep_reg @ circ1.prep_reg;
              out_regs = circ1.out_regs;
              flag_reg = circ0.flag_reg @ circ1.flag_reg;
              garb_reg = circ0.garb_reg @ circ1.garb_reg;
              used_wires;
              gate = circ0.gate @& circ1.gate;
            }
      end;
  }

let ( +& ) = circuit_sequence

let circuit_adjoint (cs : circuit_spec) : circuit_spec =
  {
    in_sizes = cs.out_sizes;
    out_sizes = cs.in_sizes;
    circ_fun =
      begin
        fun in_regs used_wires ->
          assert (List.map List.length in_regs = cs.out_sizes);
          let temp_regs, temp_used_wires =
            List.split (List.map (fresh_int_list used_wires) cs.in_sizes)
          in
          let temp_used_wires =
            List.fold_left IntSet.union IntSet.empty temp_used_wires
          in
          let circ = cs.circ_fun temp_regs temp_used_wires in
            assert (circ.garb_reg = []);
            let rewiring =
              IntMap.of_seq
                (List.to_seq
                   (List.combine
                      (List.flatten circ.out_regs)
                      (List.flatten in_regs)))
            in
            let used_wires =
              IntSet.diff temp_used_wires
                (IntSet.of_list (List.flatten circ.out_regs))
            in
            let rewire_list =
              List.map (fun i -> int_map_find_or_keep i rewiring)
            in
              {
                prep_reg = rewire_list circ.flag_reg;
                out_regs = List.map rewire_list temp_regs;
                flag_reg = rewire_list circ.prep_reg;
                garb_reg = [];
                used_wires;
                gate = unitary_adjoint (unitary_rewire circ.gate rewiring);
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
          fun in_regs used_wires ->
            let in_reg =
              match in_regs with
              | [in_reg] -> in_reg
              | _ -> assert false
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
                  let circ0 = cs0.circ_fun [in_reg_min] used_wires in
                  let circ1 =
                    cs1.circ_fun [prep_min @ in_reg_diff] circ0.used_wires
                  in
                    (circ0, circ1)
                else
                  let circ0 =
                    cs0.circ_fun [prep_min @ in_reg_diff] used_wires
                  in
                  let circ1 = cs1.circ_fun [in_reg_min] circ0.used_wires in
                    (circ0, circ1)
              in
                assert (
                  List.length circ0.garb_reg = 0
                  && List.length circ1.garb_reg = 0);
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
                          unitary_swap_regs in_reg_diff prep_diff )
                      @& Controlled
                           ( [ctrlbit],
                             [in_size0 <= in_size1],
                             unitary_swap_regs in_reg_min prep_min )
                      @& Controlled ([ctrlbit], [false], circ0.gate)
                      @& Controlled ([ctrlbit], [true], circ1.gate)
                      @& Controlled
                           ( [ctrlbit],
                             [not (out_size0 <= out_size1)],
                             unitary_swap_regs out_reg_min_in_min
                               out_reg_min_in_max );
                  }
        end;
    }

(*
Distributivity isomorphism:
H(t) * (H(t0) + H(t1)) is isomorphic to (H(t) * H(t0)) + (H(t) * H(t1))
*)
let circuit_distributivity (t : exprtype) (t0 : exprtype) (t1 : exprtype) :
    circuit_spec =
  let in_sizes = [type_size t; type_size (SumType (t0, t1))] in
  let out_size = type_size (SumType (ProdType (t, t0), ProdType (t, t1))) in
    {
      in_sizes;
      out_sizes = [out_size];
      circ_fun =
        begin
          fun in_reg used_wires ->
            assert (List.map List.length in_reg = in_sizes);
            let t_reg, rest_reg =
              match in_reg with
              | [t_reg; rest_reg] -> (t_reg, rest_reg)
              | _ -> assert false
            in
              {
                prep_reg = [];
                out_regs = [(List.hd rest_reg :: t_reg) @ List.tl rest_reg];
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
          fun in_reg used_wires ->
            assert (List.map List.length in_reg = in_sizes);
            let circ = cs.circ_fun in_reg used_wires in
            let used_wires = circ.used_wires in
            let new_prep, used_wires = fresh_int_list used_wires out_size in
            let signal_bit = List.hd new_prep in
            let fresh_prep = List.tl new_prep in
              {
                prep_reg = (signal_bit :: circ.prep_reg) @ fresh_prep;
                out_regs = [signal_bit :: List.concat circ.out_regs];
                flag_reg = [];
                garb_reg = circ.flag_reg @ circ.garb_reg @ fresh_prep;
                used_wires;
                gate =
                  circ.gate @& PauliX signal_bit
                  @& Controlled
                       ( circ.flag_reg,
                         list_constant false (List.length circ.flag_reg),
                         PauliX signal_bit
                         @& unitary_swap_regs
                              (List.concat circ.out_regs)
                              fresh_prep );
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
          fun in_reg used_wires ->
            assert (List.map List.length in_reg = in_sizes);
            let circ = cs.circ_fun in_reg used_wires in
              assert (circ.garb_reg = []);
              let used_wires = circ.used_wires in
              let signal_bit, used_wires = fresh_int_list used_wires 1 in
              let signal_bit = List.hd signal_bit in
                {
                  prep_reg = signal_bit :: circ.prep_reg;
                  out_regs =
                    [(signal_bit :: List.concat circ.out_regs) @ circ.flag_reg];
                  flag_reg = [];
                  garb_reg = [];
                  used_wires;
                  gate =
                    circ.gate @& PauliX signal_bit
                    @& Controlled
                         ( circ.flag_reg,
                           list_constant false (List.length circ.flag_reg),
                           PauliX signal_bit );
                }
        end;
    }

let context_index (d : context) (x : string) : int =
  list_index ( = ) (List.map fst (StringMap.bindings d)) x

let context_index_restriction (d : context) (fv : StringSet.t) : int list =
  List.map (context_index d)
    (List.filter
       (fun x -> StringSet.mem x fv)
       (List.map fst (StringMap.bindings d)))

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
  let reg_map_out = StringMap.filter (fun x _ -> StringSet.mem x fv) reg_map in
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
          fun in_regs used_wires ->
            let in_reg =
              match in_regs with
              | [in_reg] when List.length in_reg = in_size -> in_reg
              | _ -> assert false
            in
            let out0, out1 = context_reg_partition d in_reg fv in
              {
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

let rec compile_inter_expr_to_circuit (circ : circuit) (iv : inter_valuation)
    (used_wires : IntSet.t) (ie : inter_expr) :
    circuit * inter_valuation * IntSet.t =
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
      let op_circ = op_cs.circ_fun ev_regs used_wires in
      let used_wires = op_circ.used_wires in
      let res_circ =
        {
          prep_reg = circ.prep_reg @ op_circ.prep_reg;
          out_regs = op_circ.out_regs;
          flag_reg = circ.flag_reg @ op_circ.flag_reg;
          garb_reg = circ.garb_reg @ op_circ.garb_reg;
          used_wires;
          gate = Sequence (circ.gate, op_circ.gate);
        }
      in
      let iv' =
        StringMap.filter (fun x _ -> not (StringSet.mem x argset)) iv
      in
      let iv_add = List.combine target op_circ.out_regs in
      let iv'' =
        match map_merge false iv' (StringMap.of_seq (List.to_seq iv_add)) with
        | SomeE iv'' -> iv''
        | NoneE err -> failwith err
      in
        (res_circ, iv'', used_wires)

and compile_inter_expr_list_to_circuit (circ : circuit) (iv : inter_valuation)
    (used_wires : IntSet.t) (iel : inter_expr list) =
  match iel with
  | [] -> (circ, iv, used_wires)
  | ielh :: ielt -> begin
      let circ', iv', used_wires' =
        compile_inter_expr_to_circuit circ iv used_wires ielh
      in
        compile_inter_expr_list_to_circuit circ' iv' used_wires' ielt
    end

and compile_inter_op_to_circuit (op : inter_op) : circuit_spec =
  match op with
  | IIdentity t -> circuit_identity t
  | IU3 (theta, phi, lambda) -> circuit_u3 theta phi lambda
  | IGphase (t, theta) -> circuit_gphase t theta
  | ILeft (t0, t1) -> circuit_left t0 t1
  | IRight (t0, t1) -> circuit_right t0 t1
  | IPair (t0, t1) -> circuit_pair t0 t1
  | IShare t -> circuit_share (type_size t)
  | IContextShare d -> circuit_share (context_size d)
  | IAdjoint op' -> circuit_adjoint (compile_inter_op_to_circuit op')
  | IContextPartition (d, fv) -> circuit_context_partition d fv
  | IContextMerge (d0, d1) -> circuit_context_merge d0 d1
  | ILambda (out_size, args, iel, ret) -> begin
      let argnames = List.map fst args in
      let argsizes = List.map snd args in
        {
          in_sizes = argsizes;
          out_sizes = [out_size];
          circ_fun =
            begin
              fun in_regs used_wires ->
                let iv =
                  StringMap.of_seq
                    (List.to_seq (List.combine argnames in_regs))
                in
                let init_circ =
                  {
                    prep_reg = [];
                    out_regs = in_regs;
                    flag_reg = [];
                    garb_reg = [];
                    used_wires;
                    gate = Identity;
                  }
                in
                let circ, iv, _ =
                  compile_inter_expr_list_to_circuit init_circ iv used_wires
                    iel
                in
                let ret_regs = List.map (fun x -> StringMap.find x iv) ret in
                  {
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
        inter_lambda tsize [("g", gsize); ("d", dsize)] [] ["g"; "d"]
      end
    | Var x -> begin
        match StringMap.bindings d with
        | [] -> begin
            let xreg, grest = map_partition g (StringSet.singleton x) in
              inter_lambda tsize
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
                ["g"; "res"]
          end
        | [(x', _)] when x' = x -> begin
            inter_lambda tsize [("g", gsize); ("d", dsize)] [] ["g"; "d"]
          end
        | _ -> failwith "Error in Var semantics"
      end
    | Qpair (e0, e1) -> begin
        let t0, t1 =
          match t with
          | ProdType (t0, t1) -> (t0, t1)
          | _ -> assert false
        in
        let fv0 = free_vars e0 in
        let fv1 = free_vars e1 in
        let fv01 = StringSet.inter fv0 fv1 in
        let d01, d_xor = map_partition d fv01 in
        let d0 = map_restriction d fv0 in
        let d1 = map_restriction d fv1 in
        let op0 = compile_pure_expr_to_inter_op g d0 e0 in
        let op1 = compile_pure_expr_to_inter_op g d1 e1 in
          inter_lambda tsize
            [("g", gsize); ("d", dsize)]
            [
              inter_letapp ["d01"; "d_xor"] (IContextPartition (d, fv01)) ["d"];
              inter_letapp ["d0"; "d1"]
                (IContextPartition (d_xor, fv0))
                ["d_xor"];
              inter_letapp ["d01"; "d01*"] (IContextShare d01) ["d01"];
              inter_letapp ["d01_0"] (IContextMerge (d01, d0)) ["d01"; "d0"];
              inter_letapp ["d01*_1"] (IContextMerge (d01, d1)) ["d01*"; "d1"];
              inter_letapp ["g"; "t0"] op0 ["g"; "d01_0"];
              inter_letapp ["g"; "t1"] op1 ["g"; "d01*_1"];
              inter_letapp ["res"] (IPair (t0, t1)) ["t0"; "t1"];
            ]
            ["g"; "res"]
      end
    | Ctrl _ -> failwith "TODO"
    | Try _ -> failwith "Try is not a pure expression"
    | Apply (f, e') -> begin
        let e'_op = compile_pure_expr_to_inter_op g d e' in
        let f_op = compile_pure_prog_to_inter_op f in
          inter_lambda tsize
            [("g", gsize); ("d", dsize)]
            [
              inter_letapp ["g"; "t*"] e'_op ["g"; "d"];
              inter_letapp ["res"] f_op ["t*"];
            ]
            ["g"; "res"]
      end

and compile_pure_prog_to_inter_op (f : prog) : inter_op =
  match f with
  | U3 (theta, phi, lambda) -> IU3 (theta, phi, lambda)
  | Left (t0, t1) -> ILeft (t0, t1)
  | Right (t0, t1) -> IRight (t0, t1)
  | Lambda (e, t, e') -> begin
      let out_t =
        match prog_type_check f with
        | SomeE (Coherent (_, out_t)) -> out_t
        | SomeE (Channel _) ->
            failwith "Attempted pure compilation for mixed program"
        | NoneE err -> failwith err
      in
        match context_check StringMap.empty t e with
        | NoneE err -> failwith err
        | SomeE d -> begin
            let e_op = compile_pure_expr_to_inter_op StringMap.empty d e in
            let e'_op = compile_pure_expr_to_inter_op StringMap.empty d e' in
              inter_lambda (type_size out_t)
                [("t", type_size t)]
                [
                  inter_letapp ["d"] (IAdjoint e_op) ["t"];
                  inter_letapp ["res"] e'_op ["d"];
                ]
                ["res"]
          end
    end
  | Gphase (t, theta) -> IGphase (t, theta)

let expr_compile (e : expr) : unitary * int =
  let op = compile_pure_expr_to_inter_op StringMap.empty StringMap.empty e in
  let cs = compile_inter_op_to_circuit op in
  let circ = cs.circ_fun [[]; []] IntSet.empty in
  let nqubits = IntSet.cardinal circ.used_wires in
  let gate = unitary_remove_identities circ.gate in
    (gate, nqubits)
