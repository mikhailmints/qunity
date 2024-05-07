open Util
open Syntax
open Typechecking

type unitary =
  | Identity
  | PauliX of int
  | Had of int
  | U3Gate of (int * float * float * float)
  | Controlled of (int list * bool list * unitary)
  | Sequence of (unitary * unitary)

let ( @& ) a b = Sequence (a, b)

type circuit = {
  prep_reg : int list;
  out_reg : int list;
  flag_reg : int list;
  garb_reg : int list;
  used_wires : IntSet.t;
  gate : unitary;
}

type circuit_spec = {
  in_size : int;
  out_size : int;
  prep_size : int;
  flag_size : int;
  garb_size : int;
  circ_fun : int list -> IntSet.t -> circuit;
}

let rec unitary_adjoint (u : unitary) : unitary =
  match u with
  | Identity -> Identity
  | PauliX i -> PauliX i
  | Had i -> Had i
  | U3Gate (i, theta, phi, lambda) -> U3Gate (i, -.theta, -.lambda, -.phi)
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
      U3Gate (int_map_find_or_keep i rewiring, -.theta, -.phi, -.lambda)
  | Controlled (l, bl, u0) ->
      Controlled
        ( List.map (fun i -> int_map_find_or_keep i rewiring) l,
          bl,
          unitary_rewire u0 rewiring )
  | Sequence (u0, u1) ->
      Sequence (unitary_rewire u0 rewiring, unitary_rewire u1 rewiring)

(*
Share a register to another one by applying CNOT gates for each qubit.
*)
let rec unitary_share (reg0 : int list) (reg1 : int list) : unitary =
  match (reg0, reg1) with
  | [], [] -> Identity
  | h1 :: t1, h2 :: t2 ->
      Controlled ([h1], [true], PauliX h2) @& unitary_share t1 t2
  | _ -> invalid_arg "Registers must be of the same size."

(*
SWAP gate as 3 CNOTs
*)
let unitary_swap (b0 : int) (b1 : int) : unitary =
  Controlled ([b0], [true], PauliX b1)
  @& Controlled ([b1], [true], PauliX b0)
  @& Controlled ([b0], [true], PauliX b1)

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

let circuit_left (t0 : exprtype) (t1 : exprtype) : circuit_spec =
  let in_size = type_size t0 in
  let out_size = type_size (SumType (t0, t1)) in
  let prep_size = out_size - in_size in
    {
      in_size;
      out_size;
      prep_size;
      flag_size = 0;
      garb_size = 0;
      circ_fun =
        begin
          fun in_reg used_wires ->
            assert (List.length in_reg = in_size);
            let prep, used_wires = fresh_int_list used_wires prep_size in
              {
                prep_reg = prep;
                out_reg = (List.hd prep :: in_reg) @ List.tl prep;
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
      in_size;
      out_size;
      prep_size;
      flag_size = 0;
      garb_size = 0;
      circ_fun =
        begin
          fun in_reg used_wires ->
            assert (List.length in_reg = in_size);
            let prep, used_wires = fresh_int_list used_wires prep_size in
              {
                prep_reg = prep;
                out_reg = (List.hd prep :: in_reg) @ List.tl prep;
                flag_reg = [];
                garb_reg = [];
                used_wires;
                gate = PauliX (List.hd prep);
              }
        end;
    }

let circuit_product (cs0 : circuit_spec) (cs1 : circuit_spec) : circuit_spec =
  let in_size = cs0.in_size + cs1.in_size in
  let out_size = cs0.out_size + cs1.out_size in
    {
      in_size;
      out_size;
      prep_size = cs0.prep_size + cs1.prep_size;
      flag_size = cs0.flag_size + cs1.flag_size;
      garb_size = cs0.garb_size + cs1.garb_size;
      circ_fun =
        begin
          fun in_reg used_wires ->
            assert (List.length in_reg = in_size);
            let in_reg0, in_reg1 = list_split_at_i in_reg cs0.in_size in
            let circ0 = cs0.circ_fun in_reg0 used_wires in
            let circ1 = cs1.circ_fun in_reg1 circ0.used_wires in
            let used_wires = circ1.used_wires in
              {
                prep_reg = circ0.prep_reg @ circ1.prep_reg;
                out_reg = circ0.out_reg @ circ1.out_reg;
                flag_reg = circ0.flag_reg @ circ1.flag_reg;
                garb_reg = circ0.garb_reg @ circ1.garb_reg;
                used_wires;
                gate = circ0.gate @& circ1.gate;
              }
        end;
    }

let circuit_sequence (cs0 : circuit_spec) (cs1 : circuit_spec) : circuit_spec =
  let in_size = cs0.in_size in
  let out_size = cs1.out_size in
    {
      in_size;
      out_size;
      prep_size = cs0.prep_size + cs1.prep_size;
      flag_size = cs0.flag_size + cs1.flag_size;
      garb_size = cs0.garb_size + cs1.garb_size;
      circ_fun =
        begin
          fun in_reg used_wires ->
            assert (List.length in_reg = in_size);
            let circ0 = cs0.circ_fun in_reg used_wires in
            let circ1 = cs1.circ_fun circ0.out_reg circ0.used_wires in
            let used_wires = circ1.used_wires in
              {
                prep_reg = circ0.prep_reg @ circ1.prep_reg;
                out_reg = circ1.out_reg;
                flag_reg = circ0.flag_reg @ circ1.flag_reg;
                garb_reg = circ0.garb_reg @ circ1.garb_reg;
                used_wires;
                gate = circ0.gate @& circ1.gate;
              }
        end;
    }

let circuit_adjoint (cs : circuit_spec) : circuit_spec =
  let in_size = cs.out_size in
  let out_size = cs.in_size in
    {
      in_size;
      out_size;
      prep_size = cs.flag_size;
      flag_size = cs.prep_size;
      garb_size = 0;
      circ_fun =
        begin
          fun in_reg used_wires ->
            assert (List.length in_reg = in_size);
            let temp_reg, temp_used_wires =
              fresh_int_list used_wires cs.in_size
            in
            let circ = cs.circ_fun temp_reg temp_used_wires in
              assert (circ.garb_reg = []);
              let rewiring =
                IntMap.of_seq (List.to_seq (List.combine circ.out_reg in_reg))
              in
              let used_wires =
                IntSet.diff temp_used_wires (IntSet.of_list circ.out_reg)
              in
              let rewire_list =
                List.map (fun i -> int_map_find_or_keep i rewiring)
              in
                {
                  prep_reg = rewire_list circ.flag_reg;
                  out_reg = rewire_list temp_reg;
                  flag_reg = rewire_list circ.prep_reg;
                  garb_reg = [];
                  used_wires;
                  gate = unitary_adjoint (unitary_rewire circ.gate rewiring);
                }
        end;
    }

let circuit_dirsum (cs0 : circuit_spec) (cs1 : circuit_spec) : circuit_spec =
  let in_size = 1 + max cs0.in_size cs1.in_size in
  let out_size = 1 + max cs0.out_size cs0.out_size in
    {
      in_size;
      out_size;
      prep_size = max cs0.prep_size cs1.prep_size;
      flag_size = min cs0.flag_size cs1.flag_size;
      garb_size = 0;
      circ_fun =
        begin
          fun in_reg used_wires ->
            assert (List.length in_reg = in_size);
            let ctrlbit = List.hd in_reg in
            let in_reg = List.tl in_reg in
            let in_reg0, _ = list_split_at_i in_reg cs0.in_size in
            let in_reg1, _ = list_split_at_i in_reg cs1.in_size in
            let circ0 = cs0.circ_fun in_reg0 used_wires in
            (* Note: it's ok for them to reuse the same wires *)
            let circ1 = cs1.circ_fun in_reg1 used_wires in
              assert (circ0.garb_reg = [] && circ1.garb_reg = []);
              let used_wires =
                IntSet.union circ0.used_wires circ1.used_wires
              in
                {
                  prep_reg = int_list_union circ0.prep_reg circ1.prep_reg;
                  out_reg =
                    ctrlbit :: int_list_union circ0.out_reg circ1.out_reg;
                  flag_reg =
                    int_list_intersection circ0.flag_reg circ1.flag_reg;
                  garb_reg = [];
                  used_wires;
                  gate =
                    Controlled ([ctrlbit], [false], circ0.gate)
                    @& Controlled ([ctrlbit], [true], circ1.gate);
                }
        end;
    }

let circuit_distributivity (t : exprtype) (t0 : exprtype) (t1 : exprtype) :
    circuit_spec =
  let in_size = type_size (ProdType (t, SumType (t0, t1))) in
  let out_size = in_size in
    {
      in_size;
      out_size;
      prep_size = 0;
      flag_size = 0;
      garb_size = 0;
      circ_fun =
        begin
          fun in_reg used_wires ->
            assert (List.length in_reg = in_size);
            let t_reg, rest_reg = list_split_at_i in_reg (type_size t) in
              {
                prep_reg = [];
                out_reg = (List.hd rest_reg :: t_reg) @ List.tl rest_reg;
                flag_reg = [];
                garb_reg = [];
                used_wires;
                gate = Identity;
              }
        end;
    }

let circuit_mixed_error_handling (cs : circuit_spec) : circuit_spec =
  let in_size = cs.in_size in
  let out_size = 1 + cs.out_size in
    {
      in_size;
      out_size;
      prep_size = 1 + cs.prep_size + cs.out_size;
      flag_size = 0;
      garb_size = cs.flag_size + cs.garb_size + cs.out_size;
      circ_fun =
        begin
          fun in_reg used_wires ->
            assert (List.length in_reg = in_size);
            let circ = cs.circ_fun in_reg used_wires in
            let used_wires = circ.used_wires in
            let new_prep, used_wires =
              fresh_int_list used_wires (1 + cs.out_size)
            in
            let signal_bit = List.hd new_prep in
            let fresh_prep = List.tl new_prep in
              {
                prep_reg = (signal_bit :: circ.prep_reg) @ fresh_prep;
                out_reg = signal_bit :: circ.out_reg;
                flag_reg = [];
                garb_reg = circ.flag_reg @ circ.garb_reg @ fresh_prep;
                used_wires;
                gate =
                  circ.gate @& PauliX signal_bit
                  @& Controlled
                       ( circ.flag_reg,
                         list_constant false (List.length circ.flag_reg),
                         PauliX signal_bit
                         @& unitary_swap_regs circ.out_reg fresh_prep );
              }
        end;
    }

let circuit_pure_error_handling (cs : circuit_spec) : circuit_spec =
  let in_size = cs.in_size in
  let out_size = 1 + cs.out_size in
    {
      in_size;
      out_size;
      prep_size = 1 + cs.prep_size;
      flag_size = 0;
      garb_size = 0;
      circ_fun =
        begin
          fun in_reg used_wires ->
            assert (List.length in_reg = in_size);
            let circ = cs.circ_fun in_reg used_wires in
              assert (circ.garb_reg = []);
              let used_wires = circ.used_wires in
              let signal_bit, used_wires = fresh_int_list used_wires 1 in
              let signal_bit = List.hd signal_bit in
                {
                  prep_reg = signal_bit :: circ.prep_reg;
                  out_reg = (signal_bit :: circ.out_reg) @ circ.flag_reg;
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

let circuit_purification (cs : circuit_spec) : circuit_spec =
  let in_size = cs.in_size in
  let out_size = cs.out_size + cs.garb_size in
    {
      in_size;
      out_size;
      prep_size = cs.prep_size;
      flag_size = cs.flag_size;
      garb_size = 0;
      circ_fun =
        begin
          fun in_reg used_wires ->
            assert (List.length in_reg = in_size);
            let circ = cs.circ_fun in_reg used_wires in
            let used_wires = circ.used_wires in
              {
                prep_reg = circ.prep_reg;
                out_reg = circ.out_reg @ circ.garb_reg;
                flag_reg = circ.flag_reg;
                garb_reg = [];
                used_wires;
                gate = circ.gate;
              }
        end;
    }
