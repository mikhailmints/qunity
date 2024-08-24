open Util
open Reals
open Syntax
open Typechecking
open Gate

let debug_mode = ref false
let annotation_mode = ref false
let post_optimize = ref true

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

type instantiation_settings = {
  reset_flag : bool;
  reset_garb : bool;
  iso : bool;
}

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
  | INothing
  | IEmpty
  | IPrepReg of int
  | IIdentity of exprtype
  | ISizeIdentity of int
  | IAnnotation of int list * string
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
  | IMarkAsIso of bool * inter_op
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

type binary_tree = Leaf | Node of binary_tree * binary_tree

let rec tree_size (tree : binary_tree) : int =
  match tree with
  | Leaf -> 1
  | Node (l, r) -> tree_size l + tree_size r

let rec tree_height (tree : binary_tree) : int =
  match tree with
  | Leaf -> 0
  | Node (l, r) -> 1 + max (tree_height l) (tree_height r)

let rec tree_multiply (tree0 : binary_tree) (l_tree1 : binary_tree list) :
    binary_tree =
  match (tree0, l_tree1) with
  | Leaf, [tree1] -> tree1
  | Leaf, _ -> failwith "Mismatch when multiplying trees"
  | Node (tree0_l, tree0_r), _ -> begin
      let l_tree1_l, l_tree1_r = list_split_at_i l_tree1 (tree_size tree0_l) in
        Node (tree_multiply tree0_l l_tree1_l, tree_multiply tree0_r l_tree1_r)
    end

(* Alias for ILambda. *)
let inter_lambda (name : string) (iso : bool) (arglist : (string * int) list)
    (body : inter_com list) (ret : (string * int) list) : inter_op =
  IMarkAsIso (iso, ILambda (name, arglist, body, ret))

(* Alias for construcing an inter_com for clarity. *)
let inter_letapp (target : string list) (op : inter_op) (args : string list) :
    inter_com =
  (target, op, args)

let inter_comment (message : string) =
  inter_letapp []
    (if !annotation_mode then
       IAnnotation ([], message)
     else
       INothing)
    []

(* Add annotations describing the (low-level) roles of the registers to a circuit. *)
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
    IntSet.diff (IntSet.of_list (List.flatten in_regs)) used_wires
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
        begin
          if settings.iso && settings.reset_flag then
            (* Note that this label may not necessarily be placed
               in the location where the qubits of interest are initialized - this
               might be part of the input rather than the prep register. However,
               this is ok, because in the post-processing stage, the labels will
               be shifted as far left as possible before the qubit removal optimization
               procedure is started. *)
            gate_label_reg_for_potential_deletion circ.flag_reg
          else
            Identity
        end
        @& begin
             if settings.reset_garb then
               gate_label_reg_for_potential_deletion circ.garb_reg
             else
               Identity
           end
        @& circ.gate
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
A circuit that takes in nothing and outputs nothing.
*)
let circuit_nothing : circuit_spec =
  {
    in_sizes = [];
    out_sizes = [];
    circ_fun =
      begin
        fun in_regs used_wires _ ->
          expect_sizes "circuit_nothing" [] in_regs;
          ( {
              name = "nothing";
              in_regs;
              prep_reg = [];
              out_regs = [];
              flag_reg = [];
              garb_reg = [];
              gate = Identity;
            },
            used_wires )
      end;
  }

(*
A circuit that takes in nothing and outputs an empty register.
*)
let circuit_empty : circuit_spec =
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
A circuit that takes in nothing and outputs an zeroed register of a
given size.
*)
let circuit_prep_reg (size : int) : circuit_spec =
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

let circuit_annotation (sizes : int list) (s : string) =
  {
    in_sizes = sizes;
    out_sizes = sizes;
    circ_fun =
      begin
        fun in_regs used_wires _ ->
          expect_sizes "circuit_annotation" sizes in_regs;
          ( {
              name = "annotation \"" ^ s ^ "\"";
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
        fun in_regs used_wires settings ->
          expect_sizes "circuit_adjoint" cs.out_sizes in_regs;
          let temp_regs, temp_used_wires =
            fresh_int_lists used_wires cs.in_sizes
          in
          let circ, used_wires =
            build_circuit cs temp_regs temp_used_wires
              { settings with reset_flag = false; reset_garb = false }
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
                    (int_list_diff circ1.prep_reg (List.flatten in_regs));
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
          { reset_flag = false; reset_garb = false; iso = false }
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
          { reset_flag = false; reset_garb = false; iso = false }
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

let circuit_mark_as_iso (iso : bool) (cs : circuit_spec) : circuit_spec =
  {
    in_sizes = cs.in_sizes;
    out_sizes = cs.out_sizes;
    circ_fun =
      begin
        fun in_regs used_wires settings ->
          build_circuit cs in_regs used_wires
            (if iso = true then { settings with iso = true } else settings)
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
      let in_regs =
        List.map
          (fun x ->
            match StringMap.find_opt x iv with
            | Some reg -> reg
            | None ->
                failwith
                  (Printf.sprintf
                     "Variable %s not found: target = %s, args = %s" x
                     (string_of_list (fun s -> s) target)
                     (string_of_list (fun s -> s) args)))
          args
      in
        if !debug_mode then
          Printf.printf "(pre) Apply: args = %s, in_regs = %s, target = %s\n"
            (string_of_list (fun s -> s) args)
            (string_of_list (string_of_list string_of_int) in_regs)
            (string_of_list (fun s -> s) target);
        let op_cs = compile_inter_op_to_circuit op in
        let op_circ, used_wires =
          build_circuit op_cs in_regs used_wires settings
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
          if !debug_mode then
            Printf.printf
              "Apply %s: args = %s, in_regs = %s, target = %s, out_regs = %s\n"
              op_circ.name
              (string_of_list (fun s -> s) args)
              (string_of_list (string_of_list string_of_int) in_regs)
              (string_of_list (fun s -> s) target)
              (string_of_list (string_of_list string_of_int) op_circ.out_regs);
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
  | INothing -> circuit_nothing
  | IEmpty -> circuit_empty
  | IPrepReg size -> circuit_prep_reg size
  | ISizeIdentity size -> circuit_identity size
  | IIdentity t -> circuit_identity (type_size t)
  | IAnnotation (sizes, s) -> circuit_annotation sizes s
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
  | IMarkAsIso (iso, op') ->
      circuit_mark_as_iso iso (compile_inter_op_to_circuit op')
  | IContextPartition (d, fv) -> circuit_context_partition d fv
  | IContextMerge (d0, d1) -> circuit_context_merge d0 d1
  | IAdjoint (ISequence (op0, op1)) ->
      compile_inter_op_to_circuit (ISequence (IAdjoint op1, IAdjoint op0))
  | IAdjoint (IDirsum (op0, op1)) ->
      compile_inter_op_to_circuit (IDirsum (IAdjoint op0, IAdjoint op1))
  | IAdjoint (IMarkAsIso (_, op')) ->
      compile_inter_op_to_circuit (IAdjoint op')
  | IAdjoint (ILambda (name, args, icl, ret)) -> begin
      let icl' =
        List.rev_map
          (fun (target, op', args) -> (args, IAdjoint op', target))
          icl
      in
        compile_inter_op_to_circuit (ILambda (name, ret, icl', args))
    end
  | IAdjoint op' -> circuit_adjoint (compile_inter_op_to_circuit op')
  | ILambda (name, args, icl, ret) -> begin
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
                if !debug_mode then
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
                  compile_inter_com_list_to_circuit init_circ used_wires iv icl
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
                      int_list_intersection (List.flatten ret_regs)
                        circ.flag_reg
                      <> []
                      || int_list_intersection (List.flatten ret_regs)
                           circ.garb_reg
                         <> []
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

let rec dirsum_op_list (tree : binary_tree) (l : inter_op list) : inter_op =
  match (tree, l) with
  | _, [] -> failwith "Expected nonempty list"
  | Leaf, [op] -> op
  | Leaf, _ -> failwith "Expected non-leaf"
  | _, [_] -> failwith "Expected leaf"
  | Node (t_left, t_right), _ -> begin
      let n_left = tree_size t_left in
      let l_left, l_right = list_split_at_i l n_left in
        IDirsum (dirsum_op_list t_left l_left, dirsum_op_list t_right l_right)
    end

let dirsum_op_by_tree (tree : binary_tree) (op : inter_op) =
  dirsum_op_list tree (List.map (fun _ -> op) (range (tree_size tree)))

let rec dirsum_op_list_leveled (tree : binary_tree) (h : int)
    (l : inter_op list) : inter_op =
  match (tree, h, l) with
  | _, _, [] -> failwith "Expected nonempty list"
  | Leaf, 0, [op] -> op
  | Leaf, h, [op] ->
      IDirsum (dirsum_op_list_leveled Leaf (h - 1) [op], IIdentity Qunit)
  | Leaf, _, _ -> failwith "Expected non-leaf"
  | _, _, [_] -> failwith "Expected leaf"
  | Node (t_left, t_right), h, _ -> begin
      let n_left = tree_size t_left in
      let l_left, l_right = list_split_at_i l n_left in
        IDirsum
          ( dirsum_op_list_leveled t_left (h - 1) l_left,
            dirsum_op_list_leveled t_right (h - 1) l_right )
    end

let rec dirsum_type_list (tree : binary_tree) (l : exprtype list) : exprtype =
  match (tree, l) with
  | _, [] -> failwith "Expected nonempty list"
  | Leaf, [t] -> t
  | Leaf, _ -> failwith "Expected non-leaf"
  | _, [_] -> failwith "Expected leaf"
  | Node (t_left, t_right), _ -> begin
      let n_left = tree_size t_left in
      let l_left, l_right = list_split_at_i l n_left in
        SumType
          (dirsum_type_list t_left l_left, dirsum_type_list t_right l_right)
    end

let dirsum_type_by_tree (tree : binary_tree) (t : exprtype) =
  dirsum_type_list tree (List.map (fun _ -> t) (range (tree_size tree)))

let rec binary_tree_of_type (stop : exprtype) (t : exprtype) : binary_tree =
  match t with
  | _ when t = stop -> Leaf
  | SumType (t0, t1) ->
      Node (binary_tree_of_type stop t0, binary_tree_of_type stop t1)
  | _ -> Leaf

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
      inter_lambda "big_distr_right" false
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
      inter_lambda "big_distr_left" false
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

let fake_type_of_context_list (tree : binary_tree) (l : context list) :
    exprtype =
  dirsum_type_list tree (List.map fake_type_of_context l)

let rec big_context_distr_op (tree : binary_tree) (l : context list)
    (d : context) : inter_op =
  match (tree, l) with
  | _, [] -> failwith "Expected nonempty list"
  | Leaf, [g] -> IContextMerge (g, d)
  | Leaf, _ -> failwith "Expected non-leaf"
  | _, [_] -> failwith "Expected leaf"
  | Node (tree0, tree1), _ -> begin
      let l0, l1 = list_split_at_i l (tree_size tree0) in
      let tl0 = fake_type_of_context_list tree0 l0 in
      let tl1 = fake_type_of_context_list tree1 l1 in
      let td = fake_type_of_context d in
        IDistrRight (tl0, tl1, td)
        @&& IDirsum
              ( IAdjoint (IPair (tl0, td)) @&& big_context_distr_op tree0 l0 d,
                IAdjoint (IPair (tl1, td)) @&& big_context_distr_op tree1 l1 d
              )
    end

let rec tree_level_op (tree : binary_tree) (h : int) (l : int list) : inter_op
    =
  match (tree, l) with
  | Leaf, [s] -> begin
      inter_lambda "tree_level" true
        [("t", s)]
        [
          inter_letapp ["padding"] (IPrepReg h) [];
          inter_letapp ["res"] (ISizePair (h, s)) ["padding"; "t"];
        ]
        [("res", s + h)]
    end
  | Leaf, _ -> failwith "Expected non-leaf"
  | Node (tree0, tree1), _ ->
      let l0, l1 = list_split_at_i l (tree_size tree0) in
        IDirsum (tree_level_op tree0 (h - 1) l0, tree_level_op tree1 (h - 1) l1)

let rec compile_spanning_to_inter_op (sp : spanning_proof) :
    inter_op * binary_tree * context list =
  match sp with
  | SVoid -> (IIdentity Void, Leaf, [])
  | SUnit -> (IIdentity Qunit, Leaf, [StringMap.empty])
  | SVar (x, t) -> (IIdentity t, Leaf, [StringMap.singleton x t])
  | SSum (_, _, sp0, sp1, _, _) -> begin
      let sp0_op, tree0, gl0 = compile_spanning_to_inter_op sp0 in
      let sp1_op, tree1, gl1 = compile_spanning_to_inter_op sp1 in
        (IDirsum (sp0_op, sp1_op), Node (tree0, tree1), gl0 @ gl1)
    end
  | SPair (t0, t1, sp0, l, _, _) -> begin
      let op0, tree0, gl0 = compile_spanning_to_inter_op sp0 in
      let l_op1, l_tree1, l_gl1 =
        list_split3 (List.map compile_spanning_to_inter_op l)
      in
      let l_gl_res =
        List.map
          (fun (g0, gl1) ->
            List.map (fun g1 -> map_merge_noopt false g0 g1) gl1)
          (List.combine gl0 l_gl1)
      in
      let gl_res = List.flatten l_gl_res in
      let tree_res = tree_multiply tree0 l_tree1 in
      let t_res =
        dirsum_type_list tree_res (List.map fake_type_of_context gl_res)
      in
      let tree0_height = tree_height tree0 in
      let l_op1_sum = dirsum_op_list_leveled tree0 tree0_height l_op1 in
      let level_op0 =
        tree_level_op tree0 (tree_height tree0) (List.map context_size gl0)
      in
      let gl0_max_size = int_list_max (List.map context_size gl0) in
      let gl1_max_size =
        int_list_max (List.map context_size (List.flatten l_gl1))
      in
      let tree1_max_height = int_list_max (List.map tree_height l_tree1) in
      let l_level_op1 =
        List.map
          (fun (tree1, gl1) ->
            tree_level_op tree1 tree1_max_height (List.map context_size gl1))
          (List.combine l_tree1 l_gl1)
      in
      let level_op1_sum =
        dirsum_op_list_leveled tree0 tree0_height l_level_op1
      in
      let l_adj_level_op1 =
        List.map
          (fun (tree1, gl1) ->
            IAdjoint
              (tree_level_op tree1 tree1_max_height
                 (List.map (fun _ -> gl0_max_size + gl1_max_size) gl1)))
          (List.combine l_tree1 l_gl1)
      in
      let adj_level_op1 =
        dirsum_op_list_leveled tree0 tree0_height l_adj_level_op1
      in
      let adj_level_op0 =
        IAdjoint
          (tree_level_op tree0 (tree_height tree0)
             (List.map
                (fun tree1 -> tree_height tree1 + gl0_max_size + gl1_max_size)
                l_tree1))
      in
      let l_final_merge_op =
        List.map
          (fun (g0, gl1) ->
            List.map
              (fun g1 ->
                begin
                  let g0_size = context_size g0 in
                  let g1_size = context_size g1 in
                    inter_lambda "SPair_final_merge" true
                      [("g0,blank0,g1,blank1", gl0_max_size + gl1_max_size)]
                      [
                        inter_letapp ["g0,blank0"; "g1,blank1"]
                          (IAdjoint (ISizePair (gl0_max_size, gl1_max_size)))
                          ["g0,blank0,g1,blank1"];
                        inter_letapp ["g0"; "blank0"]
                          (IAdjoint
                             (ISizePair (g0_size, gl0_max_size - g0_size)))
                          ["g0,blank0"];
                        inter_letapp ["g1"; "blank1"]
                          (IAdjoint
                             (ISizePair (g1_size, gl1_max_size - g1_size)))
                          ["g1,blank1"];
                        inter_letapp []
                          (IAdjoint (IPrepReg (gl0_max_size - g0_size)))
                          ["blank0"];
                        inter_letapp []
                          (IAdjoint (IPrepReg (gl1_max_size - g1_size)))
                          ["blank1"];
                        inter_letapp ["g0g1"]
                          (IContextMerge (g0, g1))
                          ["g0"; "g1"];
                      ]
                      [("g0g1", g0_size + g1_size)]
                end)
              gl1)
          (List.combine gl0 l_gl1)
      in
      let final_merge_op =
        dirsum_op_list tree0
          (List.map
             (fun (tree1, l_op) -> dirsum_op_list tree1 l_op)
             (List.combine l_tree1 l_final_merge_op))
      in
        (* if gl0_max_size = 0 then begin
             let adj_level_op0_alt =
               IAdjoint
                 (tree_level_op tree0 (tree_height tree0)
                    (List.map
                       (fun (tree1, gl1) ->
                         type_size
                           (dirsum_type_list tree1
                              (List.map fake_type_of_context gl1)))
                       (List.combine l_tree1 l_gl1)))
             in
               ( inter_lambda "SPair" true
                   [("t0,t1", type_size (ProdType (t0, t1)))]
                   [
                     inter_letapp ["t0"; "t1"]
                       (IAdjoint (IPair (t0, t1)))
                       ["t0,t1"];
                     inter_letapp ["LR0_index"] op0 ["t0"];
                     inter_letapp ["LR0_index,t1"]
                       (ISizePair (tree0_height, type_size t1))
                       ["LR0_index"; "t1"];
                     inter_letapp ["sumLR0(sumR1(g1))"] l_op1_sum ["LR0_index,t1"];
                     inter_letapp ["sumR0(sumR1(g1))"] adj_level_op0_alt
                       ["sumLR0(sumR1(g1))"];
                   ]
                   [("sumR0(sumR1(g1))", type_size t_res)],
                 tree_res,
                 gl_res )
           end
           else *)
        ( inter_lambda "SPair" true
            [("t0,t1", type_size (ProdType (t0, t1)))]
            [
              inter_letapp ["t0"; "t1"] (IAdjoint (IPair (t0, t1))) ["t0,t1"];
              inter_letapp ["sumR0(g0)"] op0 ["t0"];
              inter_letapp ["sumLR0(g0)"] level_op0 ["sumR0(g0)"];
              inter_letapp ["LR0_index"; "LR0_data"]
                (IAdjoint (ISizePair (tree0_height, gl0_max_size)))
                ["sumLR0(g0)"];
              inter_letapp ["LR0_index,t1"]
                (ISizePair (tree0_height, type_size t1))
                ["LR0_index"; "t1"];
              inter_letapp ["sumLR0(sumR1(g1))"] l_op1_sum ["LR0_index,t1"];
              inter_letapp ["sumLR0(sumLR1(g1))"] level_op1_sum
                ["sumLR0(sumR1(g1))"];
              inter_letapp
                ["LR0_index,LR1_index"; "LR1_data"]
                (IAdjoint
                   (ISizePair (tree0_height + tree1_max_height, gl1_max_size)))
                ["sumLR0(sumLR1(g1))"];
              inter_letapp
                ["LR0_index,LR1_index,LR0_data"]
                (ISizePair (tree0_height + tree1_max_height, gl0_max_size))
                ["LR0_index,LR1_index"; "LR0_data"];
              inter_letapp
                ["sumLR0(sumLR1(g0,_,g1,_))"]
                (ISizePair
                   ( tree0_height + tree1_max_height + gl0_max_size,
                     gl1_max_size ))
                ["LR0_index,LR1_index,LR0_data"; "LR1_data"];
              inter_letapp
                ["sumLR0(sumR1(g0,_,g1,_))"]
                adj_level_op1
                ["sumLR0(sumLR1(g0,_,g1,_))"];
              inter_letapp
                ["sumR0(sumR1(g0,_,g1,_))"]
                adj_level_op0
                ["sumLR0(sumR1(g0,_,g1,_))"];
              inter_letapp ["sumR0(sumR1(g0g1))"] final_merge_op
                ["sumR0(sumR1(g0,_,g1,_))"];
            ]
            [("sumR0(sumR1(g0g1))", type_size t_res)],
          tree_res,
          gl_res )
    end

let rec ortho_select_op (t : exprtype) (tree : binary_tree)
    (selection : bool list) : inter_op * binary_tree =
  let n = List.length selection in
  let n' = List.length (List.filter (fun x -> x) selection) in
    if n = n' then
      (IIdentity t, tree)
    else if n' = 0 then
      failwith "Expected at least one true"
    else
      match selection with
      | [] -> failwith "Expected nonempty list"
      | [true] -> (IIdentity t, tree)
      | _ -> begin
          match (t, tree) with
          | SumType (t0, t1), Node (tree0, tree1) -> begin
              let sel0, sel1 = list_split_at_i selection (tree_size tree0) in
                if not (List.mem true sel0) then
                  let op1, tree1' = ortho_select_op t1 tree1 sel1 in
                    (IAdjoint (IRight (t0, t1)) @&& op1, tree1')
                else if not (List.mem true sel1) then
                  let op0, tree0' = ortho_select_op t0 tree0 sel0 in
                    (IAdjoint (ILeft (t0, t1)) @&& op0, tree0')
                else
                  let op0, tree0' = ortho_select_op t0 tree0 sel0 in
                  let op1, tree1' = ortho_select_op t1 tree1 sel1 in
                    (IDirsum (op0, op1), Node (tree0', tree1'))
            end
          | _ -> failwith "Expected sum type"
        end

let compile_ortho_to_inter_op (orp : ortho_proof) : inter_op * binary_tree =
  let sp, selection = orp in
  let span_op, span_tree, gl = compile_spanning_to_inter_op sp in
  let select_op, ortho_tree =
    ortho_select_op
      (dirsum_type_list span_tree (List.map fake_type_of_context gl))
      span_tree selection
  in
    (span_op @&& select_op, ortho_tree)

let rec compile_single_erasure_to_inter_op (x : string) (xtype : exprtype)
    (erp : erasure_proof) =
  let xsize = type_size xtype in
    match erp with
    | EVar t -> begin
        let tsize = type_size t in
          inter_lambda "EVar" false
            [("x", xsize); ("t", tsize)]
            [inter_letapp ["res"] (IAdjoint (IShare t)) ["t"; "x"]]
            [("res", tsize)]
      end
    | EPair0 (t0, t1, erp') -> begin
        let tsize = type_size (ProdType (t0, t1)) in
        let op' = compile_single_erasure_to_inter_op x xtype erp' in
          inter_lambda "EPair0" false
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
          inter_lambda "EPair1" false
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
        inter_lambda "erasure_empty" false
          [("d", dsize); ("t", tsize)]
          [inter_letapp [] (IAdjoint IEmpty) ["d"]]
          [("t", tsize)]
      end
    | (x, erp) :: l' -> begin
        let xtype = StringMap.find x d in
        let rest_d = StringMap.remove x d in
        let rest_op = compile_erasure_to_inter_op rest_d t l' in
        let cur_op = compile_single_erasure_to_inter_op x xtype erp in
          inter_lambda "erasure" false
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
        inter_lambda "TUnit" true
          [("g", gsize); ("d", dsize)]
          []
          [("g", gsize); ("d", tsize)]
      end
    | TCvar (t, g, x) -> begin
        let xreg, grest = map_partition g (StringSet.singleton x) in
          inter_lambda "TCvar" true
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
        inter_lambda "TQvar" true
          [("g", gsize); ("d", dsize)]
          []
          [("g", gsize); ("d", tsize)]
      end
    | TPurePair (t0, t1, _, d, d0, d1, e0, e1, iso) -> begin
        let op0 = compile_pure_expr_to_inter_op e0 in
        let op1 = compile_pure_expr_to_inter_op e1 in
          inter_lambda "TPurePair" iso
            [("g", gsize); ("dd0d1", dsize)]
            [
              inter_comment "Starting TPurePair";
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
              inter_comment "Finished TPurePair";
            ]
            [("g", gsize); ("res", tsize)]
      end
    | TCtrl (_, t1, g, g', d, d', e, l, orp, erp_map, iso) -> begin
        let n = List.length l in
        let gg'dd' = map_merge_noopt false g_whole d_whole in
          if n = 0 then
            inter_lambda "TCtrl_Void" iso
              [("gg*", gsize); ("dd*", dsize)]
              [
                inter_comment "Starting TCtrl";
                inter_letapp [] (IAlwaysErr dsize) ["dd*"];
                inter_letapp ["res"] IEmpty [];
                inter_comment "Finished TCtrl";
              ]
              [("gg*", gsize); ("res", 0)]
          else
            let e_op = compile_mixed_expr_to_inter_op e in
            let gjs = List.map fst3 l in
            let ej's = List.map trd3 l in
            let ortho_op, ortho_tree = compile_ortho_to_inter_op orp in
            let ej'_sum_op =
              dirsum_op_list ortho_tree
                (List.map
                   (fun (ej', gj) ->
                     make_pure_op_take_one_reg
                       (map_merge_noopt false g_whole gj)
                       d_whole t1
                       (compile_pure_expr_to_inter_op ej'))
                   (List.combine ej's gjs))
            in
            let erase_op =
              compile_erasure_to_inter_op d t1 (StringMap.bindings erp_map)
            in
            let fake_context_sumtype =
              fake_type_of_context_list ortho_tree
                (List.map (fun gj -> map_merge_noopt false g_whole gj) gjs)
            in
              inter_lambda "TCtrl" iso
                [("gg*", gsize); ("dd*", dsize)]
                [
                  inter_comment "Starting TCtrl";
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
                  inter_letapp ["sum(gj)"] ortho_op ["t0"];
                  inter_letapp ["gg*"] (IContextMerge (g, g')) ["g"; "g*"];
                  inter_letapp ["dd*"] (IContextMerge (d, d')) ["d"; "d*"];
                  inter_letapp ["gg*dd*"]
                    (IContextMerge (g_whole, d_whole))
                    ["gg*"; "dd*"];
                  inter_letapp ["sum(gg*gjdd*)"]
                    (big_context_distr_op ortho_tree gjs gg'dd')
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
                    (IAdjoint (big_context_distr_op ortho_tree gjs g_whole))
                    ["sum(gg*gj)"];
                  inter_letapp ["g"; "g*"]
                    (IContextPartition (g_whole, map_dom g))
                    ["gg*"];
                  inter_letapp ["t0"] (IAdjoint ortho_op) ["sum(gj)"];
                  inter_letapp ["g0d0"] (IAdjoint (IPurify e_op))
                    ["t0"; "garb"];
                  inter_letapp ["g0"; "d0"]
                    (IAdjoint (IContextMerge (g, d)))
                    ["g0d0"];
                  inter_letapp ["g"] (IAdjoint (IContextShare g)) ["g"; "g0"];
                  inter_letapp ["t1"] erase_op ["d0"; "t1"];
                  inter_letapp ["gg*"] (IContextMerge (g, g')) ["g"; "g*"];
                  inter_comment "Finished TCtrl";
                ]
                [("gg*", gsize); ("t1", tsize)]
      end
    | TPureApp (_, _, _, _, f, e', iso) -> begin
        let e'_op = compile_pure_expr_to_inter_op e' in
        let f_op = compile_pure_prog_to_inter_op f in
          inter_lambda "TPureApp" iso
            [("g", gsize); ("d", dsize)]
            [
              inter_comment "Starting TPureApp";
              inter_letapp ["g"; "t*"] e'_op ["g"; "d"];
              inter_letapp ["res"] f_op ["t*"];
              inter_comment "Finished TPureApp";
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
          inter_lambda "TMix" false
            [("d", dsize)]
            [
              inter_comment "Starting TMix";
              inter_letapp ["g"] IEmpty [];
              inter_letapp ["g"; "res"] tp'_op ["g"; "d"];
              inter_letapp [] (IAdjoint IEmpty) ["g"];
              inter_comment "Finished TMix";
            ]
            [("res", tsize)]
      end
    | TMixedPair (t0, t1, d, d0, d1, e0, e1, iso) -> begin
        let op0 = compile_mixed_expr_to_inter_op e0 in
        let op1 = compile_mixed_expr_to_inter_op e1 in
          inter_lambda "TMixedPair" iso
            [("dd0d1", dsize)]
            [
              inter_comment "Starting TMixedPair";
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
              inter_comment "Finished TMixedPair";
            ]
            [("res", tsize)]
      end
    | TTry (t, d0, _, e0, e1, iso) -> begin
        let op0 = compile_mixed_expr_to_inter_op e0 in
        let op1 = compile_mixed_expr_to_inter_op e1 in
        let iso0 = is_iso_mixed_expr_proof e0 in
        let iso1 = is_iso_mixed_expr_proof e1 in
          if iso0 then
            compile_mixed_expr_to_inter_op e0
          else if iso1 then
            inter_lambda "TTry" iso
              [("d", dsize)]
              [
                inter_comment "Starting TTry";
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
                inter_comment "Finished TTry";
              ]
              [("t", tsize)]
          else
            inter_lambda "TTry" iso
              [("d", dsize)]
              [
                inter_comment "Starting TTry";
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
                inter_comment "Finished TTry";
              ]
              [("t", tsize)]
      end
    | TMixedApp (_, _, _, f, e, iso) -> begin
        let e_op = compile_mixed_expr_to_inter_op e in
        let f_op = compile_mixed_prog_to_inter_op f in
          inter_lambda "TMixedApp" iso
            [("d", dsize)]
            [
              inter_comment "Starting TMixedApp";
              inter_letapp ["t*"] e_op ["d"];
              inter_letapp ["res"] f_op ["t*"];
              inter_comment "Finished TMixedApp";
            ]
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
  | TPureAbs (t, t', _, e, e', iso) -> begin
      let e_op = compile_pure_expr_to_inter_op e in
      let e'_op = compile_pure_expr_to_inter_op e' in
        inter_lambda "TPureAbs" iso
          [("t", type_size t)]
          [
            inter_comment "Starting TPureAbs";
            inter_letapp ["g"] IEmpty [];
            inter_letapp ["g"; "d"] (IAdjoint e_op) ["g"; "t"];
            inter_letapp ["g"; "res"] e'_op ["g"; "d"];
            inter_letapp [] (IAdjoint IEmpty) ["g"];
            inter_comment "Finished TPureAbs";
          ]
          [("res", type_size t')]
    end
  | TRphase (t, e, r0, r1) -> begin
      let _, d = context_of_pure_expr_proof e in
      let e_op = compile_pure_expr_to_inter_op e in
      let e_op_no_g = make_pure_op_take_one_reg StringMap.empty d t e_op in
        IMarkAsIso (true, IRphase (t, e_op_no_g, r0, r1))
    end

(*
Compilation of a mixed program into the intermediate representation.
*)
and compile_mixed_prog_to_inter_op (tp : mixed_prog_typing_proof) : inter_op =
  match tp with
  | TChannel tp' -> compile_pure_prog_to_inter_op tp'
  | TMixedAbs (t, t', d, d0, e, e', iso) -> begin
      let dd0 = map_merge_noopt false d d0 in
      let fve' = map_dom d in
      let e_op = compile_pure_expr_to_inter_op e in
      let e_op_no_g = make_pure_op_take_one_reg StringMap.empty dd0 t e_op in
      let e'_op = compile_mixed_expr_to_inter_op e' in
        inter_lambda "TMixedAbs" iso
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
let expr_compile (e : expr) : gate * int * int list =
  let tp = mixed_type_check_noopt e in
  let op = compile_mixed_expr_to_inter_op tp in
  let cs = compile_inter_op_to_circuit op in
  let circ, _ =
    build_circuit cs [[]] IntSet.empty
      { reset_flag = true; reset_garb = true; iso = false }
  in
  let out_reg =
    match circ.out_regs with
    | [out_reg] -> out_reg
    | _ -> failwith "Expected single out reg"
  in
  let gate =
    gate_combine_and_distribute_controls (gate_remove_identities circ.gate)
  in
  let gate, out_reg =
    if !post_optimize then
      gate_optimize gate out_reg
    else
      (gate, out_reg)
  in
  let circ = { circ with out_regs = [out_reg] } in
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
