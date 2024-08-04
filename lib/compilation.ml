open Util
open Reals
open Syntax
open Typechecking
open Gate
open Binary_tree

let debug_mode = false
let annotation_mode = ref false

(*
A circuit is a wrapper around a gate that labels the qubits by their roles as
input qubits, output qubits, prep qubits (which are initialized in the |0>
state, flag qubigs (which are expected to be zero in the end if no error is
thrown), and garbage qubits, which are measured and discarded, unless the
circuit is instantiated with the reset_garb option set to false (as it is when
constructing) a purified version of the circuit.
*)
type circuit = {
  name : string;
  in_regs : int list list;
  prep_reg : int list;
  out_regs : int list list;
  flag_reg : int list;
  garb_reg : int list;
  gate : gate;
}

type instantiation_settings = { reset_flag : bool; reset_garb : bool }

(*
A circuit specification wraps a function for building a circuit provided a
certain input register and a set of wires that were already used, as well as
an option that tells whether or not to reset the garbage register.
*)
type circuit_spec = {
  in_sizes : int list;
  out_sizes : int list;
  (* in_regs, used_wires, settings -> (circuit, updated used_wires) *)
  circ_fun :
    int list list -> IntSet.t -> instantiation_settings -> circuit * IntSet.t;
}

(*
A valuation for the intermediate representation maps strings (variable names)
to quantum registers that they represent.
*)
type inter_valuation = int list StringMap.t

(*
An operator in the intermediate representation is any one of the operators
detled in Appendix H.1 of Voichick et al, or a user-defined ILambda. The
implementation of the pre-defined operators is contained in this file. A
user-defined operator can apply a number of operators in sequence to some
registers, storing them as variables in its body.
*)
type inter_op =
  | IEmpty
  | ITestReg of int (* create empty test register for debugging purposes *)
  | IIdentity of exprtype
  | ISizeIdentity of int
  | IAnnotation of int * string
  | IU3 of real * real * real
  | IRphase of exprtype * inter_op * real * real
  | ILeft of exprtype * exprtype (* a {T0} -> left{T0, T1} a *)
  | IRight of exprtype * exprtype (* a {T1} -> right{T0, T1} a *)
  | IPair of exprtype * exprtype (* a {T0}, b {T1} -> (a, b) {T0 * T1} *)
  | ISizePair of int * int
  | IShare of exprtype (* a {T} -> [a; a] *)
  | IContextShare of context
  | IAdjoint of inter_op
  | ISequence of inter_op * inter_op
  | IDirsum of inter_op * inter_op
  | IAssoc of
      exprtype * exprtype * exprtype (* (T0 + T1) + T2 -> T0 + (T1 + T2) *)
  | IDistrLeft of
      exprtype * exprtype * exprtype (* T * (T0 + T1) -> T * T0 + T * T1 *)
  | IDistrRight of
      exprtype * exprtype * exprtype (* (T0 + T1) * T -> T0 * T + T1 * T *)
  | IMixedErr of inter_op
  | IPureErr of inter_op
  | IAlwaysErr of int
  | IDiscard of exprtype
  | IContextDiscard of context
  | IPurify of inter_op
  | IContextPartition of
      context * StringSet.t (* context -> [things in set; things not in set] *)
  | IContextMerge of context * context
    (* merge contexts assumed to be disjoint *)
  | ILambda of
      string * (string * int) list * inter_com list * (string * int) list
(* Output sizes, arg names with sizes, body, return vars. *)

(*
A command in the intermediate representation consists of the output variable
names, the and operator, input variable names. The commands represent
assigning to the output variables the operator applied to the input variables,
and they also delete the input variables since the registers will overlap in
an arbitrary way in the reassignment.
*)
and inter_com = string list * inter_op * string list

let ( @&& ) a b = ISequence (a, b)

(* Alias for ILambda. *)
let inter_lambda (name : string) (arglist : (string * int) list)
    (body : inter_com list) (ret : (string * int) list) : inter_op =
  ILambda (name, arglist, body, ret)

(* Alias for construcing an inter_com for clarity. *)
let inter_letapp (target : string list) (op : inter_op) (args : string list) :
    inter_com =
  (target, op, args)

(* Add annotations describing the roles of the registers to a circuit. *)
let annotate_circuit (circ : circuit) : circuit =
  if circ.name = "init" then
    circ
  else
    {
      circ with
      gate =
        List.fold_left ( @& ) Identity
          (List.map
             (fun (reg, i) ->
               Annotation (reg, Printf.sprintf "%s in%d" circ.name i))
             (List.combine circ.in_regs (range (List.length circ.in_regs))))
        @& Annotation (circ.prep_reg, Printf.sprintf "%s prep" circ.name)
        @& circ.gate
        @& List.fold_left ( @& ) Identity
             (List.map
                (fun (reg, i) ->
                  Annotation (reg, Printf.sprintf "%s out%d" circ.name i))
                (List.combine circ.out_regs
                   (range (List.length circ.out_regs))))
        @& Annotation (circ.flag_reg, Printf.sprintf "%s flag" circ.name)
        @& Annotation (circ.garb_reg, Printf.sprintf "%s garb" circ.name);
    }

(*
Wrapper around calling a circuit_spec's circ_fun
*)
let build_circuit (cs : circuit_spec) (in_regs : int list list)
    (used_wires : IntSet.t) (settings : instantiation_settings) :
    circuit * IntSet.t =
  if
    debug_mode
    && IntSet.diff (IntSet.of_list (List.flatten in_regs)) used_wires
       <> IntSet.empty
  then
    failwith "Input regs contain unused wires";
  let circ, used_wires = cs.circ_fun in_regs used_wires settings in
  let used_wires =
    if settings.reset_flag then
      IntSet.diff used_wires (IntSet.of_list circ.flag_reg)
    else
      used_wires
  in
  let used_wires =
    if settings.reset_garb then
      IntSet.diff used_wires (IntSet.of_list circ.garb_reg)
    else
      used_wires
  in
  let circ =
    {
      circ with
      flag_reg = (if settings.reset_flag then [] else circ.flag_reg);
      garb_reg = (if settings.reset_garb then [] else circ.garb_reg);
      gate =
        circ.gate
        @& begin
             if settings.reset_flag then
               gate_measure_reg_as_err circ.flag_reg
               @& gate_reset_reg circ.flag_reg
             else
               Identity
           end
        @& begin
             if settings.reset_garb then
               gate_reset_reg circ.garb_reg
             else
               Identity
           end;
    }
  in
    if !annotation_mode then
      (annotate_circuit circ, used_wires)
    else
      (circ, used_wires)

(*
To create a controlled version of something that is already provided as a
circuit, this should be used instead of just adding a Controlled to the
circuit's gate. This is because whatever permutation is applied to the input
qubits when rearranging them should also be controlled. This undoes the
permutation and redoes it under a control to ensure this.
*)
let controlled_circ (l : int list) (bl : bool list) (circ : circuit) : gate =
  let circ_in = List.flatten circ.in_regs @ circ.prep_reg in
  let circ_out = List.flatten circ.out_regs @ circ.flag_reg @ circ.garb_reg in
    Controlled (l, bl, circ.gate)
    @& Controlled (l, bl, gate_permute circ_in circ_out)

let rewire_circuit (circ : circuit) (source : int list) (target : int list) :
    circuit =
  let rewiring = IntMap.of_seq (List.to_seq (List.combine source target)) in
    {
      name = circ.name;
      in_regs =
        List.map (List.map (int_map_find_or_keep rewiring)) circ.in_regs;
      prep_reg = List.map (int_map_find_or_keep rewiring) circ.prep_reg;
      out_regs =
        List.map (List.map (int_map_find_or_keep rewiring)) circ.out_regs;
      flag_reg = List.map (int_map_find_or_keep rewiring) circ.flag_reg;
      garb_reg = List.map (int_map_find_or_keep rewiring) circ.garb_reg;
      gate = gate_rewire circ.gate source target;
    }

(*
Number of qubits needed to represent a value of a given size.
*)
let rec type_size (t : exprtype) =
  match t with
  | Void
  | Qunit ->
      0
  | SumType (t0, t1) -> 1 + max (type_size t0) (type_size t1)
  | ProdType (t0, t1) -> type_size t0 + type_size t1

(*
Number of qubits needed to represent a set of values corresponding to
a certain context.
*)
let context_size (d : context) =
  List.fold_left ( + ) 0
    (List.map (fun (_, t) -> type_size t) (StringMap.bindings d))

(*
Utility function for error-checking when only one in_reg should be provided
to a circuit_spec.
*)
let expect_single_in_reg (name : string) (in_regs : int list list) (size : int)
    : int list =
  match in_regs with
  | [in_reg] when List.length in_reg = size -> in_reg
  | _ ->
      failwith
        (Printf.sprintf "%s: expected input lengths [%d], got %s" name size
           (string_of_list string_of_int (List.map List.length in_regs)))

let expect_sizes (name : string) (expected : int list) (regs : int list list) :
    unit =
  if List.map List.length regs <> expected then
    failwith
      (Printf.sprintf "%s: expected sizes %s but got %s" name
         (string_of_list string_of_int expected)
         (string_of_list string_of_int (List.map List.length regs)))

(*
A circuit that takes in nothing and outputs an empty register.
*)
let circuit_empty =
  {
    in_sizes = [];
    out_sizes = [0];
    circ_fun =
      begin
        fun in_regs used_wires _ ->
          expect_sizes "circuit_empty" [] in_regs;
          ( {
              name = "empty";
              in_regs;
              prep_reg = [];
              out_regs = [[]];
              flag_reg = [];
              garb_reg = [];
              gate = Identity;
            },
            used_wires )
      end;
  }

(*
A circuit that takes in nothing and outputs an zeroed register corresponding
to a given type (used for debugging purposes).
*)
let circuit_testreg (size : int) =
  {
    in_sizes = [];
    out_sizes = [size];
    circ_fun =
      begin
        fun in_regs used_wires _ ->
          expect_sizes "circuit_testreg" [] in_regs;
          let prep, used_wires = fresh_int_list used_wires size in
            ( {
                name = "testreg";
                in_regs;
                prep_reg = prep;
                out_regs = [prep];
                flag_reg = [];
                garb_reg = [];
                gate = Identity;
              },
              used_wires )
      end;
  }

(*
The identity circuit operating on a given type.
*)
let circuit_identity (size : int) : circuit_spec =
  {
    in_sizes = [size];
    out_sizes = [size];
    circ_fun =
      begin
        fun in_regs used_wires _ ->
          ( {
              name = "identity";
              in_regs;
              prep_reg = [];
              out_regs = in_regs;
              flag_reg = [];
              garb_reg = [];
              gate = Identity;
            },
            used_wires )
      end;
  }

let circuit_annotation (size : int) (s : string) =
  {
    in_sizes = [size];
    out_sizes = [size];
    circ_fun =
      begin
        fun in_regs used_wires _ ->
          ( {
              name = "annotation";
              in_regs;
              prep_reg = [];
              out_regs = in_regs;
              flag_reg = [];
              garb_reg = [];
              gate = Annotation (List.flatten in_regs, s);
            },
            used_wires )
      end;
  }

(*
Circuit corresponding to a single-qubit gate.
*)
let circuit_u3 (theta : real) (phi : real) (lambda : real) : circuit_spec =
  {
    in_sizes = [1];
    out_sizes = [1];
    circ_fun =
      begin
        fun in_regs used_wires _ ->
          expect_sizes "circuit_u3" [1] in_regs;
          let bit = List.hd (List.hd in_regs) in
            ( {
                name = "u3";
                in_regs;
                prep_reg = [];
                out_regs = in_regs;
                flag_reg = [];
                garb_reg = [];
                gate = U3Gate (bit, theta, phi, lambda);
              },
              used_wires )
      end;
  }

(*
Circuit corresponding to a global phase gate.
*)
let circuit_gphase (t : exprtype) (theta : real) : circuit_spec =
  let size = type_size t in
    {
      in_sizes = [size];
      out_sizes = [size];
      circ_fun =
        begin
          fun in_regs used_wires _ ->
            expect_sizes "circuit_gphase" [size] in_regs;
            ( {
                name = "gphase";
                in_regs;
                prep_reg = [];
                out_regs = in_regs;
                flag_reg = [];
                garb_reg = [];
                gate = GphaseGate theta;
              },
              used_wires )
        end;
    }

(*
Lemma H.1
*)
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
              ( {
                  name = "left";
                  in_regs;
                  prep_reg = prep;
                  out_regs = [(List.hd prep :: in_reg) @ List.tl prep];
                  flag_reg = [];
                  garb_reg = [];
                  gate = Identity;
                },
                used_wires )
        end;
    }

(*
Lemma H.1
*)
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
              ( {
                  name = "right";
                  in_regs;
                  prep_reg = prep;
                  out_regs = [(List.hd prep :: in_reg) @ List.tl prep];
                  flag_reg = [];
                  garb_reg = [];
                  gate = gate_paulix (List.hd prep);
                },
                used_wires )
        end;
    }

(*
Circuit implementing the share gate, which copies classical data from the
input register. This is not quantum cloning (which is impossible), it will
only duplicate the input when it is in a basis state.
*)
let circuit_share (size : int) : circuit_spec =
  {
    in_sizes = [size];
    out_sizes = [size; size];
    circ_fun =
      begin
        fun in_regs used_wires _ ->
          let in_reg = expect_single_in_reg "circuit_share" in_regs size in
          let prep, used_wires = fresh_int_list used_wires size in
            ( {
                name = "share";
                in_regs;
                prep_reg = prep;
                out_regs = [in_reg; prep];
                flag_reg = [];
                garb_reg = [];
                gate = gate_share in_reg prep;
              },
              used_wires )
      end;
  }

(*
Circuit that takes in a value of type t0 and a value of type t1 and outputs
a value of type t0 * t1 (functionally this is just the identity circuit).
*)
let circuit_pair (size0 : int) (size1 : int) : circuit_spec =
  {
    in_sizes = [size0; size1];
    out_sizes = [size0 + size1];
    circ_fun =
      begin
        fun in_regs used_wires _ ->
          expect_sizes "circuit_pair" [size0; size1] in_regs;
          let in_reg0, in_reg1 =
            match in_regs with
            | [in_reg0; in_reg1] -> (in_reg0, in_reg1)
            | _ -> failwith "Invalid input regs"
          in
            ( {
                name = "pair";
                in_regs;
                prep_reg = [];
                out_regs = [in_reg0 @ in_reg1];
                flag_reg = [];
                garb_reg = [];
                gate = Identity;
              },
              used_wires )
      end;
  }

(*
Takes the adjoint of a circuit specification. To do this, we need to instantiate
the circuit with fresh wires and then do a rewiring using the provided input
registers.
*)
let circuit_adjoint (cs : circuit_spec) : circuit_spec =
  {
    in_sizes = cs.out_sizes;
    out_sizes = cs.in_sizes;
    circ_fun =
      begin
        fun in_regs used_wires _ ->
          expect_sizes "circuit_adjoint" cs.out_sizes in_regs;
          let temp_regs, temp_used_wires =
            fresh_int_lists used_wires cs.in_sizes
          in
          let circ, used_wires =
            build_circuit cs temp_regs temp_used_wires
              { reset_flag = false; reset_garb = false }
          in
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
              IntSet.diff used_wires
                (IntSet.of_list (List.flatten circ.out_regs))
            in
              ( {
                  name = "adjoint " ^ circ.name;
                  in_regs;
                  prep_reg =
                    List.map (int_map_find_or_keep rewiring) circ.flag_reg;
                  out_regs =
                    List.map
                      (List.map (int_map_find_or_keep rewiring))
                      temp_regs;
                  flag_reg =
                    List.map (int_map_find_or_keep rewiring) circ.prep_reg;
                  garb_reg = [];
                  gate =
                    gate_adjoint
                      (gate_rewire circ.gate
                         (List.flatten circ.out_regs)
                         (List.flatten in_regs));
                },
                used_wires )
      end;
  }

let circuit_sequence (cs0 : circuit_spec) (cs1 : circuit_spec) : circuit_spec =
  {
    in_sizes = cs0.in_sizes;
    out_sizes = cs1.out_sizes;
    circ_fun =
      begin
        fun in_regs used_wires settings ->
          let circ0, used_wires =
            build_circuit cs0 in_regs used_wires settings
          in
          let circ1, used_wires =
            build_circuit cs1 circ0.out_regs used_wires settings
          in
            ( {
                name = "sequence";
                in_regs;
                prep_reg =
                  int_list_union circ0.prep_reg
                    (int_list_diff circ1.prep_reg (List.flatten circ0.in_regs));
                out_regs = circ1.out_regs;
                flag_reg = int_list_union circ0.flag_reg circ1.flag_reg;
                garb_reg = int_list_union circ0.garb_reg circ1.garb_reg;
                gate = circ0.gate @& circ1.gate;
              },
              used_wires )
      end;
  }

(*
Circuit implementing the direct sum of two circuits.
*)
let rec circuit_dirsum (cs0 : circuit_spec) (cs1 : circuit_spec) : circuit_spec
    =
  let s0, s1, s0', s1' =
    match (cs0.in_sizes, cs1.in_sizes, cs0.out_sizes, cs1.out_sizes) with
    | [in_size0], [in_size1], [out_size0], [out_size1] ->
        (in_size0, in_size1, out_size0, out_size1)
    | _ -> assert false
  in
  let in_size = 1 + max s0 s1 in
  let out_size = 1 + max s0' s1' in
    if s0 < s1 then
      let cs_dirsum_commute = circuit_dirsum cs1 cs0 in
        {
          in_sizes = [in_size];
          out_sizes = [out_size];
          circ_fun =
            begin
              fun in_regs used_wires settings ->
                let circ, used_wires =
                  build_circuit cs_dirsum_commute in_regs used_wires settings
                in
                let signal_bit = List.hd (List.hd in_regs) in
                  ( {
                      circ with
                      gate =
                        gate_paulix signal_bit @& circ.gate
                        @& gate_paulix signal_bit;
                    },
                    used_wires )
            end;
        }
    else (* s0 >= s1 *)
      {
        in_sizes = [in_size];
        out_sizes = [out_size];
        circ_fun =
          begin
            fun in_regs used_wires settings ->
              let in_reg =
                expect_single_in_reg "circuit_dirsum" in_regs in_size
              in
              let signal_bit = List.hd in_reg in
              let s0_reg = List.tl in_reg in
              let s1_reg, rest_in_reg = list_split_at_i s0_reg s1 in
              let circ0, used_wires0 =
                build_circuit cs0 [s0_reg] used_wires
                  { settings with reset_flag = false }
              in
              (* Note that I am deliberately not updating used_wires between creating the
                 circuits! This is because we are trying to reuse the prep register as much
                 as possible. *)
              let circ1, used_wires1 =
                build_circuit cs1 [s1_reg] used_wires
                  { settings with reset_flag = false }
              in
              let name = "dirsum" in
              let p0 = List.length circ0.prep_reg in
              let p1 = List.length circ1.prep_reg in
              let f0 = List.length circ0.flag_reg in
              let f1 = List.length circ1.flag_reg in
              let circ0, circ1 =
                begin
                  if p0 >= p1 then
                    let circ1 =
                      rewire_circuit circ1 circ1.prep_reg
                        (fst (list_split_at_i circ0.prep_reg p1))
                    in
                      (circ0, circ1)
                  else
                    let circ0 =
                      rewire_circuit circ0 circ0.prep_reg
                        (fst (list_split_at_i circ1.prep_reg p0))
                    in
                      (circ0, circ1)
                end
              in
              let used_wires = IntSet.union used_wires0 used_wires1 in
              let s0'_reg, f0_reg =
                list_split_at_i (s0_reg @ circ0.prep_reg) s0'
              in
              let s1'_reg, f1_reg =
                list_split_at_i (s1_reg @ circ1.prep_reg) s1'
              in
                begin
                  (* Sanity checks *)
                  assert (s0 + p0 = s0' + f0 && s1 + p1 = s1' + f1);
                  if circ0.garb_reg <> [] || circ1.garb_reg <> [] then
                    failwith
                      "Expected garbage registers to be empty when taking \
                       direct sum";
                  let p0_set = IntSet.of_list circ0.prep_reg in
                  let p1_set = IntSet.of_list circ1.prep_reg in
                    assert (
                      IntSet.subset p0_set p1_set
                      || IntSet.subset p1_set p0_set)
                end;
                if s0' >= s1' then
                  if p0 >= p1 then (* Case 1: s0 >= s1, s0' >= s1', p0 >= p1 *)
                    let k = max 0 (f1 - f0) in
                    let k_reg, used_wires = fresh_int_list used_wires k in
                    let prep_reg = circ0.prep_reg @ k_reg in
                    let rest_prep_reg =
                      int_list_diff circ0.prep_reg circ1.prep_reg
                    in
                    let out_reg, flag_reg =
                      list_split_at_i (in_reg @ prep_reg) out_size
                    in
                      ( {
                          name;
                          in_regs;
                          prep_reg;
                          out_regs = [out_reg];
                          flag_reg;
                          garb_reg = [];
                          gate =
                            controlled_circ [signal_bit] [false] circ0
                            @& controlled_circ [signal_bit] [true] circ1
                            @& Controlled
                                 ( [signal_bit],
                                   [true],
                                   gate_permute
                                     (List.tl out_reg @ flag_reg)
                                     (s1'_reg @ rest_in_reg @ rest_prep_reg
                                    @ k_reg @ f1_reg) );
                        },
                        used_wires )
                  else (* Case 2: s0 >= s1, s0' >= s1', p0 < p1 *)
                    let k = max 0 (f1 - f0 + p0 - p1) in
                    let k_reg, used_wires = fresh_int_list used_wires k in
                    let prep_reg = circ1.prep_reg @ k_reg in
                    let out_reg, flag_reg =
                      list_split_at_i (in_reg @ prep_reg) out_size
                    in
                      ( {
                          name;
                          in_regs;
                          prep_reg;
                          out_regs = [out_reg];
                          flag_reg;
                          garb_reg = [];
                          gate =
                            controlled_circ [signal_bit] [false] circ0
                            @& controlled_circ [signal_bit] [true] circ1
                            @& Controlled
                                 ( [signal_bit],
                                   [true],
                                   gate_permute
                                     (List.tl out_reg @ flag_reg)
                                     (s1'_reg @ rest_in_reg @ k_reg @ f1_reg)
                                 );
                        },
                        used_wires )
                else if p0 >= p1 then
                  (* Case 3: s0 >= s1, s0' < s1', p0 >= p1 *)
                  let k = s1' - s0' in
                  let k_reg, used_wires = fresh_int_list used_wires k in
                  let prep_reg = circ0.prep_reg @ k_reg in
                  let rest_prep_reg =
                    int_list_diff circ0.prep_reg circ1.prep_reg
                  in
                  let out_reg, flag_reg =
                    list_split_at_i (in_reg @ prep_reg) out_size
                  in
                    ( {
                        name;
                        in_regs;
                        prep_reg;
                        out_regs = [out_reg];
                        flag_reg;
                        garb_reg = [];
                        gate =
                          controlled_circ [signal_bit] [false] circ0
                          @& controlled_circ [signal_bit] [true] circ1
                          @& Controlled
                               ( [signal_bit],
                                 [false],
                                 gate_permute
                                   (List.tl out_reg @ flag_reg)
                                   (s0'_reg @ k_reg @ f0_reg) )
                          @& Controlled
                               ( [signal_bit],
                                 [true],
                                 gate_permute
                                   (List.tl out_reg @ flag_reg)
                                   (s1'_reg @ rest_in_reg @ rest_prep_reg
                                  @ f1_reg @ k_reg) );
                      },
                      used_wires )
                else (* Case 4: s0 >= s1, s0' < s1', p0 < p1 *)
                  let k = max 0 (s1' - s0' + p0 - p1) in
                  let k_reg, used_wires = fresh_int_list used_wires k in
                  let prep_reg = circ1.prep_reg @ k_reg in
                  let rest_prep_reg =
                    int_list_diff circ1.prep_reg circ0.prep_reg
                  in
                  let out_reg, flag_reg =
                    list_split_at_i (in_reg @ prep_reg) out_size
                  in
                    ( {
                        name;
                        in_regs;
                        prep_reg;
                        out_regs = [out_reg];
                        flag_reg;
                        garb_reg = [];
                        gate =
                          controlled_circ [signal_bit] [false] circ0
                          @& controlled_circ [signal_bit] [true] circ1
                          @& Controlled
                               ( [signal_bit],
                                 [false],
                                 gate_permute
                                   (List.tl out_reg @ flag_reg)
                                   (s0'_reg @ rest_prep_reg @ k_reg @ f0_reg)
                               )
                          @& Controlled
                               ( [signal_bit],
                                 [true],
                                 gate_permute
                                   (List.tl out_reg @ flag_reg)
                                   (s1'_reg @ rest_in_reg @ f1_reg @ k_reg) );
                      },
                      used_wires )
          end;
      }

(*
Associativity isomorphism in Lemma H.6. Takes (t0 + t1) + t2 and outputs
t0 + (t1 + t2).
*)
let circuit_assoc (t0 : exprtype) (t1 : exprtype) (t2 : exprtype) :
    circuit_spec =
  (* 1 + max(1 + max(s0, s1), s2) *)
  let in_size = type_size (SumType (SumType (t0, t1), t2)) in
  (* 1 + max(s0, 1 + max(s1, s2)) *)
  let out_size = type_size (SumType (t0, SumType (t1, t2))) in
  let total_size =
    2 + max (type_size t0) (max (type_size t1) (type_size t2))
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
            let rest_reg = List.tl (List.tl in_reg) in
            let prep_reg, used_wires =
              fresh_int_list used_wires (total_size - in_size)
            in
            let out_reg, flag_reg =
              list_split_at_i
                ((signal_0v1 :: signal_01v2 :: rest_reg) @ prep_reg)
                out_size
            in
              ( {
                  name = "assoc";
                  in_regs;
                  prep_reg;
                  out_regs = [out_reg];
                  flag_reg;
                  garb_reg = [];
                  gate =
                    Controlled
                      ( [signal_01v2],
                        [true],
                        gate_rshift ((signal_0v1 :: rest_reg) @ prep_reg) )
                    @& gate_cnot signal_01v2 signal_0v1
                    @& Controlled
                         ( [signal_0v1],
                           [false],
                           gate_lshift ((signal_01v2 :: rest_reg) @ prep_reg)
                         );
                },
                used_wires )
        end;
    }

(*
Left distributivity isomorphism in Lemma H.7. Takes t * (t0 + t1) and outputs
(t * t0) + (t * t1).
*)
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
            let out_reg = (List.hd rest_reg :: t_reg) @ List.tl rest_reg in
              ( {
                  name = "distr_left";
                  in_regs;
                  prep_reg = [];
                  out_regs = [out_reg];
                  flag_reg = [];
                  garb_reg = [];
                  gate = Identity;
                },
                used_wires )
        end;
    }

(*
Right distributivity isomorphism. Takes (t0 + t1) * t and outputs
(t0 * t) + (t1 * t).
*)
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
            expect_sizes "circuit_distr_right" in_sizes in_regs;
            let sum_reg, t_reg =
              match in_regs with
              | [sum_reg; t_reg] when List.map List.length in_regs = in_sizes
                ->
                  (sum_reg, t_reg)
              | _ -> assert false
            in
            let ctrlbit = List.hd sum_reg in
            let t0t1_reg = List.tl sum_reg in
            let out_reg = sum_reg @ t_reg in
              ( {
                  name = "distr_right";
                  in_regs;
                  prep_reg = [];
                  out_regs = [out_reg];
                  flag_reg = [];
                  garb_reg = [];
                  gate =
                    begin
                      if type_size t0 > type_size t1 then
                        let t1_reg, rest_reg =
                          list_split_at_i t0t1_reg (type_size t1)
                        in
                        let perm = t1_reg @ t_reg @ rest_reg in
                          Controlled
                            ( [ctrlbit],
                              [true],
                              gate_permute (t0t1_reg @ t_reg) perm )
                      else if type_size t1 > type_size t0 then
                        let t0_reg, rest_reg =
                          list_split_at_i t0t1_reg (type_size t0)
                        in
                        let perm = t0_reg @ t_reg @ rest_reg in
                          Controlled
                            ( [ctrlbit],
                              [false],
                              gate_permute (t0t1_reg @ t_reg) perm )
                      else
                        Identity
                    end;
                },
                used_wires )
        end;
    }

(*
Lemma H.8
*)
let circuit_mixed_error_handling (cs : circuit_spec) : circuit_spec =
  let in_sizes = cs.in_sizes in
  let out_size = 1 + List.fold_left ( + ) 0 cs.out_sizes in
    {
      in_sizes;
      out_sizes = [out_size];
      circ_fun =
        begin
          fun in_regs used_wires settings ->
            expect_sizes "circuit_mixed_error_handling" in_sizes in_regs;
            let new_prep, used_wires = fresh_int_list used_wires out_size in
            let circ, used_wires =
              build_circuit cs in_regs used_wires
                { settings with reset_flag = false }
            in
            let signal_bit = List.hd new_prep in
            let fresh_prep = List.tl new_prep in
            let garb_reg =
              int_list_union circ.flag_reg
                (int_list_union circ.garb_reg fresh_prep)
            in
            let out_reg = signal_bit :: List.flatten circ.out_regs in
              ( {
                  name = "mixed_err " ^ circ.name;
                  in_regs;
                  prep_reg =
                    int_list_union (signal_bit :: circ.prep_reg) fresh_prep;
                  out_regs = [out_reg];
                  flag_reg = [];
                  garb_reg;
                  gate =
                    circ.gate
                    @& begin
                         if circ.flag_reg = [] then
                           Identity
                         else
                           gate_paulix signal_bit
                           @& gate_swap_regs
                                (List.flatten circ.out_regs)
                                fresh_prep
                           @& Controlled
                                ( circ.flag_reg,
                                  list_constant false
                                    (List.length circ.flag_reg),
                                  gate_paulix signal_bit
                                  @& gate_swap_regs
                                       (List.flatten circ.out_regs)
                                       fresh_prep )
                       end;
                },
                used_wires )
        end;
    }

(*
Lemma H.9
*)
let circuit_pure_error_handling (cs : circuit_spec) : circuit_spec =
  let flag_size =
    begin
      let in_regs, used_wires = fresh_int_lists IntSet.empty cs.in_sizes in
      let temp_circ, _ =
        build_circuit cs in_regs used_wires
          { reset_flag = false; reset_garb = false }
      in
        List.length temp_circ.flag_reg
    end
  in
  let in_sizes = cs.in_sizes in
  let out_size = 1 + List.fold_left ( + ) 0 cs.out_sizes + flag_size in
    {
      in_sizes;
      out_sizes = [out_size];
      circ_fun =
        begin
          fun in_regs used_wires settings ->
            expect_sizes "circuit_pure_error_handling" in_sizes in_regs;
            let signal_bit, used_wires = fresh_int used_wires in
            let circ, used_wires =
              build_circuit cs in_regs used_wires
                { settings with reset_flag = false }
            in
              if circ.garb_reg <> [] then
                failwith
                  "Expected empty garbage register for pure error handling";
              ( {
                  name = "pure_err " ^ circ.name;
                  in_regs;
                  prep_reg = signal_bit :: circ.prep_reg;
                  out_regs =
                    [
                      (signal_bit :: List.flatten circ.out_regs) @ circ.flag_reg;
                    ];
                  flag_reg = [];
                  garb_reg = [];
                  gate =
                    circ.gate @& gate_paulix signal_bit
                    @& begin
                         if circ.flag_reg = [] then
                           Identity
                         else
                           Controlled
                             ( circ.flag_reg,
                               list_constant false (List.length circ.flag_reg),
                               gate_paulix signal_bit )
                       end;
                },
                used_wires )
        end;
    }

let circuit_rphase (t : exprtype) (cs : circuit_spec) (r0 : real) (r1 : real) :
    circuit_spec =
  let cs_adj = circuit_adjoint cs in
  let cs_ef = circuit_pure_error_handling cs_adj in
  let cs_ef_adj = circuit_adjoint cs_ef in
  let size = type_size t in
    {
      in_sizes = [size];
      out_sizes = [size];
      circ_fun =
        begin
          fun in_regs used_wires settings ->
            expect_sizes "circuit_rphase" [size] in_regs;
            let circ_ef, used_wires =
              build_circuit cs_ef in_regs used_wires settings
            in
            let ef_out = List.hd circ_ef.out_regs in
            let signal_bit = List.hd ef_out in
            let circ_ef_adj, used_wires =
              build_circuit cs_ef_adj [ef_out] used_wires settings
            in
              ( {
                  name = "rphase " ^ circ_ef.name;
                  in_regs;
                  prep_reg = circ_ef.prep_reg;
                  out_regs = circ_ef_adj.out_regs;
                  flag_reg = circ_ef_adj.flag_reg;
                  garb_reg = [];
                  gate =
                    circ_ef.gate
                    @& begin
                         if real_equal r0 (Const 0) then
                           Identity
                         else
                           Controlled ([signal_bit], [false], GphaseGate r0)
                       end
                    @& begin
                         if real_equal r1 (Const 0) then
                           Identity
                         else
                           Controlled ([signal_bit], [true], GphaseGate r1)
                       end
                    @& circ_ef_adj.gate;
                },
                used_wires )
        end;
    }

(*
Circuit that always causes an error by creating a nonzero value in the
flag register.
*)
let circuit_always_error (size : int) =
  {
    in_sizes = [size];
    out_sizes = [];
    circ_fun =
      begin
        fun in_regs used_wires _ ->
          let in_reg =
            expect_single_in_reg "circuit_always_error" in_regs size
          in
          let flag, used_wires = fresh_int used_wires in
            ( {
                name = "always_err";
                in_regs;
                prep_reg = [flag];
                out_regs = [];
                flag_reg = in_reg @ [flag];
                garb_reg = [];
                gate = gate_paulix flag;
              },
              used_wires )
      end;
  }

(*
Discards the input by putting the input register into the garbage register.
*)
let circuit_discard (size : int) =
  {
    in_sizes = [size];
    out_sizes = [];
    circ_fun =
      begin
        fun in_regs used_wires _ ->
          let garb_reg = List.flatten in_regs in
            ( {
                name = "discard";
                in_regs;
                prep_reg = [];
                out_regs = [];
                flag_reg = [];
                garb_reg;
                gate = Identity;
              },
              used_wires )
      end;
  }

(*
Purification of a circuit - ensures that when the circuit is built, no
garbage wires are reset or reused.
*)
let circuit_purify (cs : circuit_spec) : circuit_spec =
  let garb_size =
    begin
      let in_regs, used_wires = fresh_int_lists IntSet.empty cs.in_sizes in
      let temp_circ, _ =
        build_circuit cs in_regs used_wires
          { reset_flag = false; reset_garb = false }
      in
        List.length temp_circ.garb_reg
    end
  in
    {
      in_sizes = cs.in_sizes;
      out_sizes = cs.out_sizes @ [garb_size];
      circ_fun =
        begin
          fun in_regs used_wires settings ->
            let circ, used_wires =
              build_circuit cs in_regs used_wires
                { settings with reset_garb = false }
            in
              ( {
                  name = "purified " ^ circ.name;
                  in_regs;
                  out_regs = circ.out_regs @ [circ.garb_reg];
                  prep_reg = circ.prep_reg;
                  flag_reg = circ.flag_reg;
                  garb_reg = [];
                  gate = circ.gate;
                },
                used_wires )
        end;
    }

(*
Given a context and a register containing something in the space associated
to the context, splits the register into smaller registers mapped to each
variable in the context.
*)
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

(*
Partition a register corresponding to a context into a part corresponding to
those variables that are in a specified set and those that are not.
*)
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

(*
Creates a circuit that partitions a register corresponding to a context into a
part corresponding to those variables that are in a specified set and those that
are not. These two new registers form the output registers of the circuit.
*)
let circuit_context_partition (d : context) (fv : StringSet.t) : circuit_spec =
  let in_size = context_size d in
  let d0, d1 = map_partition d fv in
    {
      in_sizes = [in_size];
      out_sizes = [context_size d0; context_size d1];
      circ_fun =
        begin
          fun in_regs used_wires _ ->
            let in_reg =
              expect_single_in_reg "circuit_context_partition" in_regs in_size
            in
            let out0, out1 = context_reg_partition d in_reg fv in
              ( {
                  name = "context_partition";
                  in_regs;
                  prep_reg = [];
                  out_regs = [out0; out1];
                  flag_reg = [];
                  garb_reg = [];
                  gate = Identity;
                },
                used_wires )
        end;
    }

(*
A circuit that merges two disjoint (important!) contexts into one. Since the
contexts are disjoint, this merge is simply the adjioint of the
partition circuit.
*)
let circuit_context_merge (d0 : context) (d1 : context) : circuit_spec =
  let d =
    match map_merge false d0 d1 with
    | SomeE d -> d
    | NoneE err -> failwith err
  in
  let fv0 = map_dom d0 in
    circuit_adjoint (circuit_context_partition d fv0)

(*
Given a circuit and an intermediate representation command, as well as an
intermediate representation valuation, applies the command's operator to the
registers of the circuit. Returns an updated circuit and valuation (which
has the new variables corresponding to the output registers of the circuit).
*)
let rec compile_inter_com_to_circuit (circ : circuit) (used_wires : IntSet.t)
    (iv : inter_valuation) (ic : inter_com) (settings : instantiation_settings)
    : circuit * IntSet.t * inter_valuation =
  let target, op, args = ic in
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
      let op_circ, used_wires =
        build_circuit op_cs ev_regs used_wires settings
      in
      let res_circ =
        {
          name = op_circ.name;
          in_regs = circ.in_regs;
          prep_reg =
            int_list_union circ.prep_reg
              (int_list_diff op_circ.prep_reg (List.flatten circ.in_regs));
          out_regs = op_circ.out_regs;
          flag_reg = int_list_union circ.flag_reg op_circ.flag_reg;
          garb_reg = int_list_union circ.garb_reg op_circ.garb_reg;
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
          (res_circ, used_wires, iv'')

(*
Applies a list of intermediate representation commands, in series, to a circuit,
updating the valuation each time.
*)
and compile_inter_com_list_to_circuit (circ : circuit) (used_wires : IntSet.t)
    (iv : inter_valuation) (icl : inter_com list)
    (settings : instantiation_settings) : circuit * IntSet.t * inter_valuation
    =
  match icl with
  | [] -> (circ, used_wires, iv)
  | iclh :: iclt -> begin
      let circ', used_wires', iv' =
        compile_inter_com_to_circuit circ used_wires iv iclh settings
      in
        compile_inter_com_list_to_circuit circ' used_wires' iv' iclt settings
    end

(*
Converts an intermediate representation operator to a circuit, calling the
previously defined circuits for most cases and calling the above functions
for the user-defined ILambda case.
*)
and compile_inter_op_to_circuit (op : inter_op) : circuit_spec =
  match op with
  | IEmpty -> circuit_empty
  | ITestReg size -> circuit_testreg size
  | ISizeIdentity size -> circuit_identity size
  | IIdentity t -> circuit_identity (type_size t)
  | IAnnotation (size, s) -> circuit_annotation size s
  | IU3 (theta, phi, lambda) -> circuit_u3 theta phi lambda
  | IRphase (t, op, r0, r1) ->
      if r0 = r1 then
        circuit_gphase t r0
      else
        circuit_rphase t (compile_inter_op_to_circuit op) r0 r1
  | ILeft (t0, t1) -> circuit_left t0 t1
  | IRight (t0, t1) -> circuit_right t0 t1
  | IPair (t0, t1) -> circuit_pair (type_size t0) (type_size t1)
  | ISizePair (s0, s1) -> circuit_pair s0 s1
  | IShare t -> circuit_share (type_size t)
  | IContextShare d -> circuit_share (context_size d)
  | IAdjoint op' -> circuit_adjoint (compile_inter_op_to_circuit op')
  | ISequence (op0, op1) ->
      circuit_sequence
        (compile_inter_op_to_circuit op0)
        (compile_inter_op_to_circuit op1)
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
  | IAlwaysErr size -> circuit_always_error size
  | IDiscard t -> circuit_discard (type_size t)
  | IContextDiscard d -> circuit_discard (context_size d)
  | IPurify op' -> circuit_purify (compile_inter_op_to_circuit op')
  | IContextPartition (d, fv) -> circuit_context_partition d fv
  | IContextMerge (d0, d1) -> circuit_context_merge d0 d1
  | ILambda (name, args, iel, ret) -> begin
      let arg_names = List.map fst args in
      let arg_sizes = List.map snd args in
      let ret_names = List.map fst ret in
      let ret_sizes = List.map snd ret in
        {
          in_sizes = arg_sizes;
          out_sizes = ret_sizes;
          circ_fun =
            begin
              fun in_regs used_wires settings ->
                if debug_mode then
                  Printf.printf
                    "ILambda %s: arg_names = %s, in_regs = %s, arg_sizes = \
                     %s, ret_sizes = %s\n"
                    name
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
                    name = "init";
                    in_regs;
                    prep_reg = [];
                    out_regs = in_regs;
                    flag_reg = [];
                    garb_reg = [];
                    gate = Identity;
                  }
                in
                let circ, used_wires, iv =
                  compile_inter_com_list_to_circuit init_circ used_wires iv iel
                    settings
                in
                  if
                    not
                      (StringSet.equal (map_dom iv)
                         (StringSet.of_list ret_names))
                  then
                    failwith
                      (Printf.sprintf
                         "ILambda %s: Returned variables do not correspond to \
                          final valuation - remaining unused variables are %s"
                         name
                         (string_of_list
                            (fun x -> x)
                            (StringSet.elements
                               (StringSet.diff (map_dom iv)
                                  (StringSet.of_list ret_names)))));
                  let ret_regs =
                    List.map (fun x -> StringMap.find x iv) ret_names
                  in
                    if
                      debug_mode
                      && (int_list_intersection (List.flatten ret_regs)
                            circ.flag_reg
                          <> []
                         || int_list_intersection (List.flatten ret_regs)
                              circ.garb_reg
                            <> [])
                    then begin
                      Printf.printf
                        "ILambda %s:\n\
                         ret_regs: %s\n\
                         flag_reg: %s\n\
                         garb_reg: %s\n"
                        name
                        (string_of_list
                           (string_of_list string_of_int)
                           ret_regs)
                        (string_of_list string_of_int circ.flag_reg)
                        (string_of_list string_of_int circ.garb_reg);
                      failwith "ret_regs overlap with flag or garb regs"
                    end;
                    ({ circ with out_regs = ret_regs }, used_wires)
            end;
        }
    end

let make_pure_op_take_one_reg (g : context) (d : context) (t : exprtype)
    (op : inter_op) =
  let gsize = context_size g in
  let tsize = type_size t in
    IAdjoint (IContextMerge (g, d)) @&& op @&& ISizePair (gsize, tsize)

let rec dirsum_op_n_times (n : int) (op : inter_op) =
  if n < 1 then
    failwith "Expected n >= 1"
  else if n = 1 then
    op
  else
    let n_left = complete_binary_left_subtree n in
    let n_right = n - n_left in
      IDirsum (dirsum_op_n_times n_left op, dirsum_op_n_times n_right op)

let rec dirsum_op_list (l : inter_op list) : inter_op =
  match l with
  | [] -> failwith "Expected nonempty list"
  | [op] -> op
  | _ -> begin
      let n_left = complete_binary_left_subtree (List.length l) in
      let l_left, l_right = list_split_at_i l n_left in
        IDirsum (dirsum_op_list l_left, dirsum_op_list l_right)
    end

let rec dirsum_type_n_times (n : int) (t : exprtype) =
  if n < 1 then
    failwith "Expected n >= 1"
  else if n = 1 then
    t
  else
    let n_left = complete_binary_left_subtree n in
    let n_right = n - n_left in
      SumType (dirsum_type_n_times n_left t, dirsum_type_n_times n_right t)

let rec dirsum_type_list (l : exprtype list) : exprtype =
  match l with
  | [] -> failwith "Expected nonempty list"
  | [t] -> t
  | _ -> begin
      let n_left = complete_binary_left_subtree (List.length l) in
      let l_left, l_right = list_split_at_i l n_left in
        SumType (dirsum_type_list l_left, dirsum_type_list l_right)
    end

let rec binary_tree_of_type (stop : exprtype) (t : exprtype) : binary_tree =
  match t with
  | _ when t = stop -> Leaf
  | SumType (t0, t1) ->
      Node (binary_tree_of_type stop t0, binary_tree_of_type stop t1)
  | _ -> Leaf

let rec transform_type_as_tree (stop : exprtype) (trans : tree_transformation)
    (t : exprtype) : exprtype =
  match t with
  | _ when t = stop -> t
  | SumType (t0, t1) -> begin
      match trans with
      | TreeIdentity -> t
      | TreeLeftRotation -> begin
          match t1 with
          | SumType (t10, t11) -> SumType (SumType (t0, t10), t11)
          | _ -> failwith "Cannot apply left rotation"
        end
      | TreeRightRotation -> begin
          match t0 with
          | SumType (t00, t01) -> SumType (t00, SumType (t01, t1))
          | _ -> failwith "Cannot apply right rotation"
        end
      | TreeLeftApply trans' ->
          SumType (transform_type_as_tree stop trans' t0, t1)
      | TreeRightApply trans' ->
          SumType (t0, transform_type_as_tree stop trans' t1)
      | TreeSequence (trans0, trans1) ->
          transform_type_as_tree stop trans1
            (transform_type_as_tree stop trans0 t)
    end
  | _ -> t

let rec inter_op_of_tree_transformation (stop : exprtype)
    (trans : tree_transformation) (t : exprtype) : inter_op =
  match trans with
  | TreeIdentity -> IIdentity t
  | TreeLeftRotation -> begin
      match t with
      | SumType (t0, SumType (t1, t2)) -> IAdjoint (IAssoc (t0, t1, t2))
      | _ -> failwith "Can't convert left rotation to inter_op"
    end
  | TreeRightRotation -> begin
      match t with
      | SumType (SumType (t0, t1), t2) -> IAssoc (t0, t1, t2)
      | _ -> failwith "Can't convert right rotation to inter_op"
    end
  | TreeLeftApply trans' -> begin
      match t with
      | SumType (t0, t1) ->
          IDirsum (inter_op_of_tree_transformation stop trans' t0, IIdentity t1)
      | _ -> failwith "Can't convert left apply to inter_op"
    end
  | TreeRightApply trans' -> begin
      match t with
      | SumType (t0, t1) ->
          IDirsum (IIdentity t0, inter_op_of_tree_transformation stop trans' t1)
      | _ -> failwith "Can't convert right apply to inter_op"
    end
  | TreeSequence (trans0, trans1) ->
      ISequence
        ( inter_op_of_tree_transformation stop trans0 t,
          inter_op_of_tree_transformation stop trans1
            (transform_type_as_tree stop trans0 t) )

let balance_dirsum_tree_op (stop : exprtype) (t : exprtype) : inter_op =
  inter_op_of_tree_transformation stop
    (balance_tree (binary_tree_of_type stop t))
    t

let rec big_distr_right_type (stop : exprtype) (t0 : exprtype) (t1 : exprtype)
    : exprtype =
  match t0 with
  | _ when t0 = stop -> ProdType (t0, t1)
  | SumType (t00, t01) ->
      SumType
        (big_distr_right_type stop t00 t1, big_distr_right_type stop t01 t1)
  | _ -> ProdType (t0, t1)

let rec big_distr_right_op (stop : exprtype) (t0 : exprtype) (t1 : exprtype) :
    inter_op =
  match t0 with
  | _ when t0 = stop -> IIdentity (ProdType (t0, t1))
  | SumType (t00, t01) -> begin
      inter_lambda "big_distr_right"
        [("(t00+t01)*t1", type_size (ProdType (t0, t1)))]
        [
          inter_letapp ["t00+t01"; "t1"]
            (IAdjoint (IPair (t0, t1)))
            ["(t00+t01)*t1"];
          inter_letapp ["(t00*t1)+(t01*t1)"]
            (IDistrRight (t00, t01, t1))
            ["t00+t01"; "t1"];
          inter_letapp ["res"]
            (IDirsum
               (big_distr_right_op stop t00 t1, big_distr_right_op stop t01 t1))
            ["(t00*t1)+(t01*t1)"];
        ]
        [("res", type_size (big_distr_right_type stop t0 t1))]
    end
  | _ -> IIdentity (ProdType (t0, t1))

let rec big_distr_left_type (stop : exprtype) (t0 : exprtype) (t1 : exprtype) :
    exprtype =
  match t1 with
  | _ when t1 = stop -> ProdType (t0, t1)
  | SumType (t10, t11) ->
      SumType (big_distr_left_type stop t0 t10, big_distr_left_type stop t0 t11)
  | _ -> ProdType (t0, t1)

let rec big_distr_left_op (stop : exprtype) (t0 : exprtype) (t1 : exprtype) :
    inter_op =
  match t1 with
  | _ when t1 = stop -> IIdentity (ProdType (t0, t1))
  | SumType (t10, t11) -> begin
      inter_lambda "big_distr_left"
        [("t0*(t10+t11)", type_size (ProdType (t0, t1)))]
        [
          inter_letapp ["t0"; "t10+t11"]
            (IAdjoint (IPair (t0, t1)))
            ["t0*(t10+t11)"];
          inter_letapp ["(t0*t10)+(t0*t11)"]
            (IDistrLeft (t0, t10, t11))
            ["t0"; "t10+t11"];
          inter_letapp ["res"]
            (IDirsum
               (big_distr_left_op stop t0 t10, big_distr_left_op stop t0 t11))
            ["(t0*t10)+(t0*t11)"];
        ]
        [("res", type_size (big_distr_left_type stop t0 t1))]
    end
  | _ -> IIdentity (ProdType (t0, t1))

let fake_type_of_context (d : context) : exprtype =
  let types = List.map snd (StringMap.bindings d) in
    List.fold_right (fun x cur -> ProdType (x, cur)) types Qunit

let fake_type_of_context_list (l : context list) : exprtype =
  dirsum_type_list (List.map fake_type_of_context l)

let rec big_context_distr_op (l : context list) (d : context) : inter_op =
  match l with
  | [] -> failwith "Expected nonempty list"
  | [g] -> IContextMerge (g, d)
  | _ -> begin
      let l0, l1 =
        list_split_at_i l (complete_binary_left_subtree (List.length l))
      in
      let tl0 = fake_type_of_context_list l0 in
      let tl1 = fake_type_of_context_list l1 in
      let td = fake_type_of_context d in
        IDistrRight (tl0, tl1, td)
        @&& IDirsum
              ( IAdjoint (IPair (tl0, td)) @&& big_context_distr_op l0 d,
                IAdjoint (IPair (tl1, td)) @&& big_context_distr_op l1 d )
    end

let rec compile_spanning_to_inter_op (sp : spanning_proof) : inter_op =
  match sp with
  | SVoid
  | SUnit ->
      IIdentity Qunit
  | SVar t -> IIdentity t
  | SSum (t0, t1, sp0, sp1, n0, n1) -> begin
      let sp0_op = compile_spanning_to_inter_op sp0 in
      let sp1_op = compile_spanning_to_inter_op sp1 in
        inter_lambda "SSum"
          [("t0+t1", type_size (SumType (t0, t1)))]
          [
            inter_letapp ["t0^(+n0)+t1^(+n1)"]
              (IDirsum (sp0_op, sp1_op))
              ["t0+t1"];
            inter_letapp
              ["(t0+t1)^(+n0)+(t0+t1)^(+n1)"]
              (IDirsum
                 ( dirsum_op_n_times n0 (ILeft (t0, t1)),
                   dirsum_op_n_times n1 (IRight (t0, t1)) ))
              ["t0^(+n0)+t1^(+n1)"];
            inter_letapp ["res"]
              (balance_dirsum_tree_op
                 (SumType (t0, t1))
                 (SumType
                    ( dirsum_type_n_times n0 (SumType (t0, t1)),
                      dirsum_type_n_times n1 (SumType (t0, t1)) )))
              ["(t0+t1)^(+n0)+(t0+t1)^(+n1)"];
          ]
          [
            ( "res",
              type_size (dirsum_type_n_times (n0 + n1) (SumType (t0, t1))) );
          ]
    end
  | SPair (t0, t1, sp0, l, m, njs) -> begin
      let op0 = compile_spanning_to_inter_op sp0 in
      let l_op = List.map compile_spanning_to_inter_op l in
      let tensor_with_id_t0 (op, nj) =
        inter_lambda "tensor_with_id_t0"
          [("t0*t1", type_size (ProdType (t0, t1)))]
          [
            inter_letapp ["t0"; "t1"] (IAdjoint (IPair (t0, t1))) ["t0*t1"];
            inter_letapp ["t1^(+nj)"] op ["t1"];
            inter_letapp ["res"]
              (IPair (t0, dirsum_type_n_times nj t1))
              ["t0"; "t1^(+nj)"];
          ]
          [("res", type_size (ProdType (t0, dirsum_type_n_times nj t1)))]
      in
      let l_op_sum =
        dirsum_op_list (List.map tensor_with_id_t0 (List.combine l_op njs))
      in
      let sum_nj = List.fold_left ( + ) 0 njs in
      let mt0 = dirsum_type_n_times m t0 in
        inter_lambda "SPair"
          [("t0*t1", type_size (ProdType (t0, t1)))]
          [
            inter_letapp ["t0"; "t1"] (IAdjoint (IPair (t0, t1))) ["t0*t1"];
            inter_letapp ["t0^(+m)"] op0 ["t0"];
            inter_letapp ["t0^(+m)*t1"]
              (IPair (dirsum_type_n_times m t0, t1))
              ["t0^(+m)"; "t1"];
            inter_letapp ["(t0*t1)^(+m)"]
              (big_distr_right_op t0 mt0 t1)
              ["t0^(+m)*t1"];
            inter_letapp ["sum(t0*t1^(+nj))"] l_op_sum ["(t0*t1)^(+m)"];
            inter_letapp ["sum((t0*t1)^(+nj))"]
              (dirsum_op_list
                 (List.map
                    (fun nj ->
                      big_distr_left_op t1 t0 (dirsum_type_n_times nj t1))
                    njs))
              ["sum(t0*t1^(+nj))"];
            inter_letapp ["res"]
              (balance_dirsum_tree_op
                 (ProdType (t0, t1))
                 (dirsum_type_list
                    (List.map
                       (fun nj -> dirsum_type_n_times nj (ProdType (t0, t1)))
                       njs)))
              ["sum((t0*t1)^(+nj))"];
          ]
          [("res", type_size (dirsum_type_n_times sum_nj (ProdType (t0, t1))))]
    end

let rec ortho_select_op (t : exprtype) (selection : bool list) :
    inter_op * exprtype =
  let n = List.length selection in
  let n' = List.length (List.filter (fun x -> x) selection) in
    if n = n' then
      (IIdentity t, t)
    else if n' = 0 then
      failwith "Expected at least one true"
    else
      match selection with
      | [] -> failwith "Expected nonempty list"
      | [true] -> (IIdentity t, t)
      | _ -> begin
          match t with
          | SumType (t0, t1) -> begin
              let sel0, sel1 =
                list_split_at_i selection (complete_binary_left_subtree n)
              in
                if not (List.mem true sel0) then
                  let op1, t1' = ortho_select_op t1 sel1 in
                    (IAdjoint (IRight (t0, t1)) @&& op1, t1')
                else if not (List.mem true sel1) then
                  let op0, t0' = ortho_select_op t0 sel0 in
                    (IAdjoint (ILeft (t0, t1)) @&& op0, t0')
                else
                  let op0, t0' = ortho_select_op t0 sel0 in
                  let op1, t1' = ortho_select_op t1 sel1 in
                    (IDirsum (op0, op1), SumType (t0', t1'))
            end
          | _ -> failwith "Expected sum type"
        end

let compile_ortho_to_inter_op (t : exprtype) (orp : ortho_proof) : inter_op =
  let sp, span_list, ortho_list = orp in
  let span_op = compile_spanning_to_inter_op sp in
  let selection = List.map (fun e -> List.mem e ortho_list) span_list in
  let sum_t = dirsum_type_n_times (List.length span_list) t in
  let select_op, sum_t' = ortho_select_op sum_t selection in
    span_op @&& select_op @&& balance_dirsum_tree_op t sum_t'

let rec compile_single_erasure_to_inter_op (x : string) (xtype : exprtype)
    (erp : erasure_proof) =
  let xsize = type_size xtype in
    match erp with
    | EVar t -> begin
        let tsize = type_size t in
          inter_lambda "EVar"
            [("x", xsize); ("t", tsize)]
            [inter_letapp ["res"] (IAdjoint (IShare t)) ["t"; "x"]]
            [("res", tsize)]
      end
    | EPair0 (t0, t1, erp') -> begin
        let tsize = type_size (ProdType (t0, t1)) in
        let op' = compile_single_erasure_to_inter_op x xtype erp' in
          inter_lambda "EPair0"
            [("x", xsize); ("t", tsize)]
            [
              inter_letapp ["t0"; "t1"] (IAdjoint (IPair (t0, t1))) ["t"];
              inter_letapp ["t0"] op' ["x"; "t0"];
              inter_letapp ["res"] (IPair (t0, t1)) ["t0"; "t1"];
            ]
            [("res", tsize)]
      end
    | EPair1 (t0, t1, erp') -> begin
        let tsize = type_size (ProdType (t0, t1)) in
        let op' = compile_single_erasure_to_inter_op x xtype erp' in
          inter_lambda "EPair1"
            [("x", xsize); ("t", tsize)]
            [
              inter_letapp ["t0"; "t1"] (IAdjoint (IPair (t0, t1))) ["t"];
              inter_letapp ["t1"] op' ["x"; "t1"];
              inter_letapp ["res"] (IPair (t0, t1)) ["t0"; "t1"];
            ]
            [("res", tsize)]
      end

let rec compile_erasure_to_inter_op (d : context) (t : exprtype)
    (l : (string * erasure_proof) list) : inter_op =
  let dsize = context_size d in
  let tsize = type_size t in
    match l with
    | [] -> begin
        inter_lambda "erasure_empty"
          [("d", dsize); ("t", tsize)]
          [inter_letapp [] (IAdjoint IEmpty) ["d"]]
          [("t", tsize)]
      end
    | (x, erp) :: l' -> begin
        let xtype = StringMap.find x d in
        let rest_d = StringMap.remove x d in
        let rest_op = compile_erasure_to_inter_op rest_d t l' in
        let cur_op = compile_single_erasure_to_inter_op x xtype erp in
          inter_lambda "erasure"
            [("d", dsize); ("t", tsize)]
            [
              inter_letapp ["x"; "rest_d"]
                (IContextPartition (d, StringSet.singleton x))
                ["d"];
              inter_letapp ["t"] rest_op ["rest_d"; "t"];
              inter_letapp ["t"] cur_op ["x"; "t"];
            ]
            [("t", tsize)]
      end

(*
Compilation of a pure expression into the intermediate representation.
This creates a circuit that takes in two registers, corresponding to the
classical and quantum contexts.
*)
let rec compile_pure_expr_to_inter_op (tp : pure_expr_typing_proof) : inter_op
    =
  let t = type_of_pure_expr_proof tp in
  let g_whole, d_whole = context_of_pure_expr_proof tp in
  let gsize = context_size g_whole in
  let dsize = context_size d_whole in
  let tsize = type_size t in
    match tp with
    | TUnit _ -> begin
        inter_lambda "TUnit"
          [("g", gsize); ("d", dsize)]
          []
          [("g", gsize); ("d", tsize)]
      end
    | TCvar (t, g, x) -> begin
        let xreg, grest = map_partition g (StringSet.singleton x) in
          inter_lambda "TCvar"
            [("g", gsize); ("d", dsize)]
            [
              inter_letapp ["x"; "rest"]
                (IContextPartition (g, StringSet.singleton x))
                ["g"];
              inter_letapp [] (IAdjoint IEmpty) ["d"];
              inter_letapp ["x"; "res"] (IShare t) ["x"];
              inter_letapp ["g"] (IContextMerge (xreg, grest)) ["x"; "rest"];
            ]
            [("g", gsize); ("res", tsize)]
      end
    | TQvar _ -> begin
        inter_lambda "TQvar"
          [("g", gsize); ("d", dsize)]
          []
          [("g", gsize); ("d", tsize)]
      end
    | TPurePair (t0, t1, _, d, d0, d1, e0, e1, _) -> begin
        let op0 = compile_pure_expr_to_inter_op e0 in
        let op1 = compile_pure_expr_to_inter_op e1 in
          inter_lambda "TPurePair"
            [("g", gsize); ("dd0d1", dsize)]
            [
              inter_letapp ["d"; "d0d1"]
                (IContextPartition (d_whole, map_dom d))
                ["dd0d1"];
              inter_letapp ["d0"; "d1"]
                (IContextPartition (map_merge_noopt false d0 d1, map_dom d0))
                ["d0d1"];
              inter_letapp ["d"; "d*"] (IContextShare d) ["d"];
              inter_letapp ["dd0"] (IContextMerge (d, d0)) ["d"; "d0"];
              inter_letapp ["d*d1"] (IContextMerge (d, d1)) ["d*"; "d1"];
              inter_letapp ["g"; "t0"] op0 ["g"; "dd0"];
              inter_letapp ["g"; "t1"] op1 ["g"; "d*d1"];
              inter_letapp ["res"] (IPair (t0, t1)) ["t0"; "t1"];
            ]
            [("g", gsize); ("res", tsize)]
      end
    | TCtrl (t0, t1, g, g', d, d', e, l, orp, erp_map, _) -> begin
        let n = List.length l in
        let gg'dd' = map_merge_noopt false g_whole d_whole in
          if n = 0 then
            inter_lambda "TCtrl_Void"
              [("gg*", gsize); ("dd*", dsize)]
              [
                inter_letapp [] (IAlwaysErr dsize) ["dd*"];
                inter_letapp ["res"] IEmpty [];
              ]
              [("gg*", gsize); ("res", 0)]
          else
            let e_op = compile_mixed_expr_to_inter_op e in
            let gjs = List.map (fun (x, _, _) -> x) l in
            let ejs = List.map (fun (_, x, _) -> x) l in
            let ej's = List.map (fun (_, _, x) -> x) l in
            let ej_adj_sum_op =
              dirsum_op_list
                (List.map
                   (fun (ej, gj) ->
                     IAdjoint
                       (make_pure_op_take_one_reg StringMap.empty gj t0
                          (compile_pure_expr_to_inter_op ej)))
                   (List.combine ejs gjs))
            in
            let ej_sum_op =
              dirsum_op_list
                (List.map
                   (fun (ej, gj) ->
                     make_pure_op_take_one_reg StringMap.empty gj t0
                       (compile_pure_expr_to_inter_op ej))
                   (List.combine ejs gjs))
            in
            let ej'_sum_op =
              dirsum_op_list
                (List.map
                   (fun (ej', gj) ->
                     make_pure_op_take_one_reg
                       (map_merge_noopt false g_whole gj)
                       d_whole t1
                       (compile_pure_expr_to_inter_op ej'))
                   (List.combine ej's gjs))
            in
            let ortho_op = compile_ortho_to_inter_op t0 orp in
            let erase_op =
              compile_erasure_to_inter_op d t1 (StringMap.bindings erp_map)
            in
            let fake_context_sumtype =
              fake_type_of_context_list
                (List.map (fun gj -> map_merge_noopt false g_whole gj) gjs)
            in
              inter_lambda "TCtrl"
                [("gg*", gsize); ("dd*", dsize)]
                [
                  inter_letapp ["g"; "g*"]
                    (IContextPartition (g_whole, map_dom g))
                    ["gg*"];
                  inter_letapp ["d"; "d*"]
                    (IContextPartition (d_whole, map_dom d))
                    ["dd*"];
                  inter_letapp ["g"; "g0"] (IContextShare g) ["g"];
                  inter_letapp ["d"; "d0"] (IContextShare d) ["d"];
                  inter_letapp ["g0d0"] (IContextMerge (g, d)) ["g0"; "d0"];
                  inter_letapp ["t0"; "garb"] (IPurify e_op) ["g0d0"];
                  inter_letapp ["t0^(+n)"] ortho_op ["t0"];
                  inter_letapp ["sum(gj)"] ej_adj_sum_op ["t0^(+n)"];
                  inter_letapp ["gg*"] (IContextMerge (g, g')) ["g"; "g*"];
                  inter_letapp ["dd*"] (IContextMerge (d, d')) ["d"; "d*"];
                  inter_letapp ["gg*dd*"]
                    (IContextMerge (g_whole, d_whole))
                    ["gg*"; "dd*"];
                  inter_letapp ["sum(gg*gjdd*)"]
                    (big_context_distr_op gjs gg'dd')
                    ["sum(gj)"; "gg*dd*"];
                  inter_letapp ["sum(gg*gj*t1)"] ej'_sum_op ["sum(gg*gjdd*)"];
                  inter_letapp ["sum(gg*gj)*t1"]
                    (IAdjoint
                       (big_distr_right_op Qunit fake_context_sumtype t1))
                    ["sum(gg*gj*t1)"];
                  inter_letapp ["sum(gg*gj)"; "t1"]
                    (IAdjoint (IPair (fake_context_sumtype, t1)))
                    ["sum(gg*gj)*t1"];
                  inter_letapp ["sum(gj)"; "gg*"]
                    (IAdjoint (big_context_distr_op gjs g_whole))
                    ["sum(gg*gj)"];
                  inter_letapp ["g"; "g*"]
                    (IContextPartition (g_whole, map_dom g))
                    ["gg*"];
                  inter_letapp ["t0^(+n)"] ej_sum_op ["sum(gj)"];
                  inter_letapp ["t0"] (IAdjoint ortho_op) ["t0^(+n)"];
                  inter_letapp ["g0d0"] (IAdjoint (IPurify e_op))
                    ["t0"; "garb"];
                  inter_letapp ["g0"; "d0"]
                    (IAdjoint (IContextMerge (g, d)))
                    ["g0d0"];
                  inter_letapp ["g"] (IAdjoint (IContextShare g)) ["g"; "g0"];
                  inter_letapp ["t1"] erase_op ["d0"; "t1"];
                  inter_letapp ["gg*"] (IContextMerge (g, g')) ["g"; "g*"];
                ]
                [("gg*", gsize); ("t1", tsize)]
      end
    | TPureApp (_, _, _, _, f, e', _) -> begin
        let e'_op = compile_pure_expr_to_inter_op e' in
        let f_op = compile_pure_prog_to_inter_op f in
          inter_lambda "TPureApp"
            [("g", gsize); ("d", dsize)]
            [
              inter_letapp ["g"; "t*"] e'_op ["g"; "d"];
              inter_letapp ["res"] f_op ["t*"];
            ]
            [("g", gsize); ("res", tsize)]
      end

(*
Compilation of a mixed expression into the intermediate representation.
This creates a circuit that takes in one register, corresponding to the
quantum context.
*)
and compile_mixed_expr_to_inter_op (tp : mixed_expr_typing_proof) : inter_op =
  let t = type_of_mixed_expr_proof tp in
  let d_whole = context_of_mixed_expr_proof tp in
  let dsize = context_size d_whole in
  let tsize = type_size t in
    match tp with
    | TMix tp' -> begin
        let tp'_op = compile_pure_expr_to_inter_op tp' in
          inter_lambda "TMix"
            [("d", dsize)]
            [
              inter_letapp ["g"] IEmpty [];
              inter_letapp ["g"; "res"] tp'_op ["g"; "d"];
              inter_letapp [] (IAdjoint IEmpty) ["g"];
            ]
            [("res", tsize)]
      end
    | TMixedPair (t0, t1, d, d0, d1, e0, e1, _) -> begin
        let op0 = compile_mixed_expr_to_inter_op e0 in
        let op1 = compile_mixed_expr_to_inter_op e1 in
          inter_lambda "TMixedPair"
            [("dd0d1", dsize)]
            [
              inter_letapp ["d"; "d0d1"]
                (IContextPartition (d_whole, map_dom d))
                ["dd0d1"];
              inter_letapp ["d0"; "d1"]
                (IContextPartition (map_merge_noopt false d0 d1, map_dom d0))
                ["d0d1"];
              inter_letapp ["d"; "d*"] (IContextShare d) ["d"];
              inter_letapp ["dd0"] (IContextMerge (d, d0)) ["d"; "d0"];
              inter_letapp ["d*d1"] (IContextMerge (d, d1)) ["d*"; "d1"];
              inter_letapp ["t0"] op0 ["dd0"];
              inter_letapp ["t1"] op1 ["d*d1"];
              inter_letapp ["res"] (IPair (t0, t1)) ["t0"; "t1"];
            ]
            [("res", tsize)]
      end
    | TTry (t, d0, _, e0, e1, _) -> begin
        let op0 = compile_mixed_expr_to_inter_op e0 in
        let op1 = compile_mixed_expr_to_inter_op e1 in
        let iso0 = is_iso_mixed_expr_proof e0 in
        let iso1 = is_iso_mixed_expr_proof e1 in
          if iso0 then
            compile_mixed_expr_to_inter_op e0
          else if iso1 then
            inter_lambda "TTry"
              [("d", dsize)]
              [
                inter_letapp ["d0"; "d1"]
                  (IContextPartition (d_whole, map_dom d0))
                  ["d"];
                inter_letapp ["t+c"] (IMixedErr op0) ["d0"];
                inter_letapp ["t"] op1 ["d1"];
                inter_letapp ["t*t+t"] (IDistrRight (t, Qunit, t)) ["t+c"; "t"];
                inter_letapp ["t"; "t+c"]
                  (IAdjoint (IDistrLeft (t, t, Qunit)))
                  ["t*t+t"];
                inter_letapp [] (IDiscard (SumType (t, Qunit))) ["t+c"];
              ]
              [("t", tsize)]
          else
            inter_lambda "TTry"
              [("d", dsize)]
              [
                inter_letapp ["d0"; "d1"]
                  (IContextPartition (d_whole, map_dom d0))
                  ["d"];
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
    | TMixedApp (_, _, _, f, e, _) -> begin
        let e_op = compile_mixed_expr_to_inter_op e in
        let f_op = compile_mixed_prog_to_inter_op f in
          inter_lambda "TMixedApp"
            [("d", dsize)]
            [inter_letapp ["t*"] e_op ["d"]; inter_letapp ["res"] f_op ["t*"]]
            [("res", tsize)]
      end

(*
Compilation of a pure program into the intermediate representation.
*)
and compile_pure_prog_to_inter_op (tp : pure_prog_typing_proof) : inter_op =
  match tp with
  | TGate (theta, phi, lambda) -> IU3 (theta, phi, lambda)
  | TLeft (t0, t1) -> ILeft (t0, t1)
  | TRight (t0, t1) -> IRight (t0, t1)
  | TPureAbs (t, t', _, e, e', _) -> begin
      let e_op = compile_pure_expr_to_inter_op e in
      let e'_op = compile_pure_expr_to_inter_op e' in
        inter_lambda "TPureAbs"
          [("t", type_size t)]
          [
            inter_letapp ["g"] IEmpty [];
            inter_letapp ["g"; "d"] (IAdjoint e_op) ["g"; "t"];
            inter_letapp ["g"; "res"] e'_op ["g"; "d"];
            inter_letapp [] (IAdjoint IEmpty) ["g"];
          ]
          [("res", type_size t')]
    end
  | TRphase (t, e, r0, r1) -> begin
      let _, d = context_of_pure_expr_proof e in
      let e_op = compile_pure_expr_to_inter_op e in
      let e_op_no_g = make_pure_op_take_one_reg StringMap.empty d t e_op in
        IRphase (t, e_op_no_g, r0, r1)
    end

(*
Compilation of a mixed program into the intermediate representation.
*)
and compile_mixed_prog_to_inter_op (tp : mixed_prog_typing_proof) : inter_op =
  match tp with
  | TChannel tp' -> compile_pure_prog_to_inter_op tp'
  | TMixedAbs (t, t', d, d0, e, e', _) -> begin
      let dd0 = map_merge_noopt false d d0 in
      let fve' = map_dom d in
      let e_op = compile_pure_expr_to_inter_op e in
      let e_op_no_g = make_pure_op_take_one_reg StringMap.empty dd0 t e_op in
      let e'_op = compile_mixed_expr_to_inter_op e' in
        inter_lambda "TMixedAbs"
          [("t", type_size t)]
          [
            inter_letapp ["d"] (IAdjoint e_op_no_g) ["t"];
            inter_letapp ["d*"; "d0"] (IContextPartition (dd0, fve')) ["d"];
            inter_letapp ["res"] e'_op ["d*"];
            inter_letapp [] (IContextDiscard d0) ["d0"];
          ]
          [("res", type_size t')]
    end

(*
Main procedure for compiling a Qunity expression into a low-level quantum
gate: into the intermediate representation, then compiling the intermediate
representation to a circuit, then applying several postprocessing steps, and
outputting the circuit's gate and additional relevant information.
*)
let expr_compile (annotate : bool) (e : expr) : gate * int * int list =
  annotation_mode := annotate;
  let tp = mixed_type_check_noopt e in
  let op = compile_mixed_expr_to_inter_op tp in
  let cs = compile_inter_op_to_circuit op in
  let circ, _ =
    build_circuit cs [[]] IntSet.empty { reset_flag = true; reset_garb = true }
  in
  let gate =
    gate_optimize
      (gate_combine_and_distribute_controls (gate_remove_identities circ.gate))
  in
  let qubits_used =
    List.of_seq
      (IntSet.to_seq
         (IntSet.union (gate_qubits_used gate)
            (IntSet.of_list (List.flatten circ.out_regs))))
  in
  let nqubits = List.length qubits_used in
  let rewire_source = qubits_used @ int_list_diff circ.prep_reg qubits_used in
  let rewire_target = range (List.length rewire_source) in
  let circ = rewire_circuit circ rewire_source rewire_target in
  let gate = gate_rewire gate rewire_source rewire_target in
  let out_reg =
    match circ.out_regs with
    | [out_reg] -> out_reg
    | _ -> failwith "Expected single out reg"
  in
    (gate, nqubits, out_reg)
