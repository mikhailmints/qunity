open Util
open Reals
open Syntax
open Typechecking
open Gate
open Binary_tree

(** If set to true, prints some helpful debugging information. *)
let debug_mode = ref false

(** If set to true, adds annotation gates to circuits. *)
let annotation_mode = ref false

(** If set to true, runs the post-processing optimization procedure on the
    compiled circuit. *)
let post_optimize = ref true

type circuit = {
  name : string;
      (** Name associated with the circuit, used for debugging purposes. *)
  in_regs : int list list;  (** List of input registers. *)
  prep_reg : int list;  (** The prep register used by the circuit. *)
  out_regs : int list list;  (** List of output registers. *)
  flag_reg : int list;  (** Flag register. *)
  garb_reg : int list;  (** Garbage register. *)
  gate : gate;  (** Gate associated with the circuit. *)
}
(** A {e circuit} is a wrapper around a gate that labels the qubits by their
    roles as input qubits, output qubits, prep qubits (which are initialized in
    the {m |0\rangle} state), flag qubits (which are expected to be
    {m |0\rangle} in the end if no error is thrown), and garbage qubits, which
    are measured and discarded, unless the circuit is instantiated with the
    [reset_garb] setting set to [false] (as it is when constructing a purified
    version of the circuit). *)

(** String representation of a circuit. *)
let string_of_circuit (circ : circuit) : string =
  Printf.sprintf
    "name: %s\n\
     in_regs: %s\n\
     prep_reg: %s\n\
     out_regs: %s\n\
     flag_reg: %s\n\
     garb_reg: %s\n\
     gate: %s\n"
    circ.name
    (string_of_list (string_of_list string_of_int) circ.in_regs)
    (string_of_list string_of_int circ.prep_reg)
    (string_of_list (string_of_list string_of_int) circ.out_regs)
    (string_of_list string_of_int circ.flag_reg)
    (string_of_list string_of_int circ.garb_reg)
    (string_of_gate circ.gate)

type instantiation_settings = {
  reset_flag : bool;
      (** Whether or not to measure and reset the flag qubits. *)
  reset_garb : bool;
      (** Whether or not to measure and reset the garbage qubits. *)
  iso : bool;
      (** Whether or not this circuit can be treated as an {e isometry}, in
          which case we can expect the flag registers to end in the
          {m |0\rangle} state, allowing for some additional post-processing
          optimizations. *)
}
(** An object containing settings that are passed to a circuit specification
    when it is instantiated into a circuit. *)

type circuit_spec = {
  in_sizes : int list;
      (** The required input register sizes that the circuit specification
          accepts. *)
  out_sizes : int list;
      (** The sizes of the output registers of the circuit specification. *)
  circ_fun :
    int list list -> IntSet.t -> instantiation_settings -> circuit * IntSet.t;
      (** Circuit instantiation function: Takes in [in_regs], [used_wires],
          [settings], and outputs ([circuit], updated [used_wires]). *)
}
(** A {e circuit specification} wraps a function for building a circuit
    provided a certain input register and a set of wires that were already
    used, as well as an option that tells whether or not to reset the garbage
    register. *)

type inter_valuation = int list StringMap.t
(** A {e valuation} for the intermediate representation maps strings (variable
    names) to quantum registers that they represent. *)

(** An {e operator} in the intermediate representation is any one of the
    operators detled in Appendix H.1 of Voichick et al, or a user-defined
    [IFunc]. The implementation of the pre-defined operators is contained in
    this file. A user-defined operator can apply a number of operators in
    sequence to some registers, storing them as variables in its body. *)
type inter_op =
  | INothing  (** Takes in nothing and outputs nothing. *)
  | IEmpty  (** Takes in nothing and outputs an empty register. *)
  | IPrepReg of int
      (** Takes in nothing and outputs a register in the {m |0\rangle} state of
          a given size. *)
  | IIdentity of exprtype
      (** Identity operator corresponding to a given type. *)
  | ISizeIdentity of int  (** Identity operator of a given size. *)
  | IAnnotation of int list * string
      (** Adds an annotation with a comment to a register. *)
  | IU3 of real * real * real  (** Single-qubit gate *)
  | IRphase of exprtype * inter_op * real * real
      (** Relative phase operator, using a given operator to define a subspace
          of the given type. *)
  | ILeft of exprtype * exprtype
      (** Left direct sum injection {m T_0 \rarr T_0 \oplus T_1}. *)
  | IRight of exprtype * exprtype
      (** Right direct sum injection {m T_1 \rarr T_0 \oplus T_1}. *)
  | IPair of exprtype * exprtype
      (** Takes in two registers corresponding to types {m T_0} and {m T_1},
          and combines them into one register corresponding to the type
          {m T_0 \otimes T_1}. *)
  | ISizePair of int * int
      (** Combines two registers of given sizes into one. *)
  | IShare of exprtype  (** The share gate for a given type. *)
  | IContextShare of context  (** The share gate for a given context. *)
  | IAdjoint of inter_op  (** Takes the adjoint of another operator. *)
  | ISequence of inter_op * inter_op
      (** Sequence of two operators applied one after the other. *)
  | IDirsum of inter_op * inter_op  (** Direct sum of two operators. *)
  | IAssoc of exprtype * exprtype * exprtype
      (** Direct sum associativity isomorphism
          {m (T_0 \oplus T1) \oplus T_2 \rarr T_0 \oplus (T_1 \oplus T_2)}. *)
  | ICommute of exprtype * exprtype
      (** Direct sum commutativity isomorphism
          {m (T_0 \oplus T1) \rarr T_1 \oplus T_0}. *)
  | IConditionalCommute of exprtype * exprtype * bool list
      (** In a direct sum tree type, commutes data lying along the same given
          path in the left and right subspace. *)
  | IDistrLeft of exprtype * exprtype * exprtype
      (** Left distributivity isomorphism
          {m T \otimes (T_0 \oplus T_1) \rarr T \otimes T_0 \oplus T \otimes T_1}. *)
  | IDistrRight of exprtype * exprtype * exprtype
      (** Right distributivity isomorphism
          {m (T_0 \oplus T1) \otimes T \rarr T_0 \otimes T \oplus T_1 \otimes T}. *)
  | IMixedErr of inter_op  (** Mixed error handling circuit. *)
  | IPureErr of inter_op  (** Pure error handling circuit. *)
  | IAlwaysErr of int  (** Circuit component that always throws an error. *)
  | IDiscard of exprtype
      (** Discards the input register, associated with a type. *)
  | IContextDiscard of context
      (** Discards the input register, associated with a context. *)
  | IPurify of inter_op  (** Purification of an operator. *)
  | IMarkAsIso of bool * bool * inter_op
      (** Marks an operator as an isometry or unitary, changing the
          corresponding instantiation settings. *)
  | IContextPartition of context * StringSet.t
      (** Partitions a register corresponding to a context into two according
          to a given set. *)
  | IContextMerge of context * context
      (** Merges contexts assumed to be disjoint, ensuring proper ordering of
          variables. *)
  | IClassicalTryCatch of inter_op * inter_op
      (** Measures the flag register output by the first operator, and if it is
          nonzero, replaces the output with that of the second operator. *)
  | IFunc of
      string * (string * int) list * inter_com list * (string * int) list
      (** A user-defined operator. Consists of a name, a list of argument names
          with sizes, a list of commands (assignments), and a list of output
          variable names with sizes. *)

and inter_com = string list * inter_op * string list
(** A {e command} in the intermediate representation consists of the output
    variable names, the operator, and input variable names. The commands
    represent assigning to the output variables the operator applied to the
    input variables, and they also delete the input variables since the
    registers will overlap in an arbitrary way in the reassignment. *)

(** Alias for [ISequence]. *)
let ( @&& ) a b = ISequence (a, b)

(** Wrapper for [IFunc]. *)
let inter_func (name : string) (arglist : (string * int) list)
    (body : inter_com list) (ret : (string * int) list) : inter_op =
  IFunc (name, arglist, body, ret)

(** Wrapper for [IFunc], also adding an option to mark it as an isometry or
    unitary. *)
let inter_func_marked (name : string) (iso : bool) (un : bool)
    (arglist : (string * int) list) (body : inter_com list)
    (ret : (string * int) list) : inter_op =
  if un && not iso then failwith "If un holds, iso must also hold";
  IMarkAsIso (iso, un, IFunc (name, arglist, body, ret))

(** Alias for construcing an [inter_com]. *)
let inter_letapp (target : string list) (op : inter_op) (args : string list) :
    inter_com =
  (target, op, args)

(** An [inter_com] creating an annotation not applied to any registers. *)
let inter_comment (message : string) =
  inter_letapp []
    (if !annotation_mode then
       IAnnotation ([], message)
     else
       INothing)
    []

(** Add annotations describing the (low-level) roles of the registers to a
    circuit. *)
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

(** Wrapper around a [circuit_spec]'s [circ_fun]. This should always be used
    instead of calling the [circ_fun] directly. This instantiates a
    [circuit_spec] into a [circuit], and it also makes sure that the
    instantiation settings are followed. If some registers are reset, the
    corresponding qubits are removed from the [used_wires] set and are thus
    {e recycled}. This also places {e potential deletion labels} onto resetting
    garbage registers, and, if [settings.iso] is true (the circuit is marked as
    an isometry), it also places them onto resetting flag registers. *)
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
            (* Note that this label may not necessarily be placed in the
               location where the qubits of interest are initialized - this
               might be part of the input rather than the prep register.
               However, this is ok, because in the post-processing stage, the
               labels will be shifted as far left as possible before the qubit
               removal optimization procedure is started. *)
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
               if settings.iso then
                 gate_label_reg_as_zero_state circ.flag_reg
               else
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

(** To create a controlled version of something that is already provided as a
    circuit, this should be used instead of just adding a [Controlled] to the
    circuit's gate. This is because whatever permutation is applied to the
    input qubits when rearranging them should also be controlled. This function
    ensures that the permutation is applied as a controlled gate. *)
let controlled_circ (l : int list) (bl : bool list) (circ : circuit) : gate =
  let circ_in = List.flatten circ.in_regs @ circ.prep_reg in
  let circ_out = List.flatten circ.out_regs @ circ.flag_reg @ circ.garb_reg in
    Controlled (l, bl, circ.gate)
    @& Controlled (l, bl, gate_permute circ_in circ_out)

(** Rewires a circuit, remapping all qubits in [source] to the corresponding
    qubits in [target]. *)
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

(** Number of qubits needed to represent a value of a given size. *)
let rec type_size (t : exprtype) =
  match t with
  | Void
  | Qunit ->
      0
  | SumType (t0, t1) -> 1 + max (type_size t0) (type_size t1)
  | ProdType (t0, t1) -> type_size t0 + type_size t1

(** Number of qubits needed to represent a set of values corresponding to a
    certain context. *)
let context_size (d : context) =
  List.fold_left ( + ) 0
    (List.map (fun (_, t) -> type_size t) (StringMap.bindings d))

(** Utility function for error-checking when only one [in_reg] should be
    provided to a [circuit_spec]. *)
let expect_single_in_reg (name : string) (in_regs : int list list) (size : int)
    : int list =
  match in_regs with
  | [in_reg] when List.length in_reg = size -> in_reg
  | _ ->
      failwith
        (Printf.sprintf "%s: expected input lengths [%d], got %s" name size
           (string_of_list string_of_int (List.map List.length in_regs)))

(** Utility function for error-checking that the provided [regs] provided match
    the [expected] sizes. *)
let expect_sizes (name : string) (expected : int list) (regs : int list list) :
    unit =
  if List.map List.length regs <> expected then
    failwith
      (Printf.sprintf "%s: expected sizes %s but got %s" name
         (string_of_list string_of_int expected)
         (string_of_list string_of_int (List.map List.length regs)))

(** A circuit that takes in nothing and outputs nothing. *)
let circuit_nothing : circuit_spec =
  {
    in_sizes = [];
    out_sizes = [];
    circ_fun =
      begin
        fun (in_regs : int list list) (used_wires : IntSet.t) _ ->
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

(** A circuit that takes in nothing and outputs an empty register. *)
let circuit_empty : circuit_spec =
  {
    in_sizes = [];
    out_sizes = [0];
    circ_fun =
      begin
        fun (in_regs : int list list) (used_wires : IntSet.t) _ ->
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

(** A circuit that takes in nothing and outputs an zeroed register of a given
    size. *)
let circuit_prep_reg (size : int) : circuit_spec =
  {
    in_sizes = [];
    out_sizes = [size];
    circ_fun =
      begin
        fun in_regs used_wires _ ->
          expect_sizes "circuit_prep_reg" [] in_regs;
          let prep, used_wires = fresh_int_list used_wires size in
            ( {
                name = "prep_reg";
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

(** The identity circuit operating on a given type. *)
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

(** Adds an annotation with a comment to a register. *)
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

(** Circuit corresponding to a single-qubit gate. *)
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

(** Circuit corresponding to a global phase gate. *)
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

(** Circuit corresponding to the left direct sum injection
    {m T_0 \rarr T_0 \oplus T_1}. *)
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

(** Circuit corresponding to the right direct sum injection
    {m T_1 \rarr T_0 \oplus T_1}. *)
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

(** Circuit implementing the share gate, which copies classical data from the
    input register. This is not quantum cloning (which is impossible) - it will
    only duplicate the input when it is in a basis state. *)
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

(** Circuit that combines two registers of given sizes into one. *)
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

(** Takes the adjoint of a circuit specification. To do this, we need to
    instantiate the circuit with fresh wires and then do a rewiring using the
    provided input registers. *)
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

(** Combines two circuit specifications in a sequence, ensuring that registers
    are properly handled in the middle where qubit recycling could occur. *)
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

(** Circuit implementing the direct sum of two circuits. *)
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
              (* Note that I am deliberately not updating used_wires between
                 creating the circuits! This is because we are trying to reuse
                 the prep register as much as possible. *)
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

(** Direct sum associativity isomorphism
    {m (T_0 \oplus T_1) \oplus T_2 \rarr T_0 \oplus (T_1 \oplus T_2)}. *)
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

(** Direct sum commutativity isomorphism
    {m (T_0 \oplus T1) \rarr T_1 \oplus T_0}. *)
let circuit_commute (t0 : exprtype) (t1 : exprtype) : circuit_spec =
  let size = type_size (SumType (t0, t1)) in
    {
      in_sizes = [size];
      out_sizes = [size];
      circ_fun =
        begin
          fun in_regs used_wires _ ->
            let in_reg = expect_single_in_reg "circuit_commute" in_regs size in
              ( {
                  name = "commute";
                  in_regs = [in_reg];
                  prep_reg = [];
                  out_regs = [in_reg];
                  flag_reg = [];
                  garb_reg = [];
                  gate = gate_paulix (List.hd in_reg);
                },
                used_wires )
        end;
    }

(** In a direct sum tree type, commutes data lying along the same given path in
    the left and right subspace. *)
let circuit_conditional_commute (t0 : exprtype) (t1 : exprtype)
    (bl : bool list) : circuit_spec =
  let size = type_size (SumType (t0, t1)) in
    {
      in_sizes = [size];
      out_sizes = [size];
      circ_fun =
        begin
          fun in_regs used_wires _ ->
            let in_reg = expect_single_in_reg "circuit_commute" in_regs size in
            let signal_qubit = List.hd in_reg in
            let l, _ = list_split_at_i (List.tl in_reg) (List.length bl) in
              ( {
                  name = "commute";
                  in_regs = [in_reg];
                  prep_reg = [];
                  out_regs = [in_reg];
                  flag_reg = [];
                  garb_reg = [];
                  gate = Controlled (l, bl, gate_paulix signal_qubit);
                },
                used_wires )
        end;
    }

(** Left distributivity isomorphism
    {m T \otimes (T_0 \oplus T_1) \rarr T \otimes T_0 \oplus T \otimes T_1}. *)
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

(** Right distributivity isomorphism
    {m (T_0 \oplus T_1) \otimes T \rarr T_0 \otimes T \oplus T_1 \otimes T}. *)
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

(** Mixed error handling circuit: given a circuit specification corresponding
    to a trace non-increasing superoperator with output space {m \mathcal{H}},
    converts it into a circuit corresponding to a trace-preserving
    superoperator with output space {m \mathcal{H} \oplus \mathbb{C}}, sending
    the error states to the new subspace. The first input register is
    explicitly treated as a classical context register, and thus is not
    included in this output space. *)
let circuit_mixed_error_handling (cs : circuit_spec) : circuit_spec =
  let in_sizes = cs.in_sizes in
  let out_sum_size = 1 + List.fold_left ( + ) 0 (List.tl cs.out_sizes) in
  let out_sizes = [List.hd cs.out_sizes; out_sum_size] in
    {
      in_sizes;
      out_sizes;
      circ_fun =
        begin
          fun in_regs used_wires settings ->
            expect_sizes "circuit_mixed_error_handling" in_sizes in_regs;
            let new_prep, used_wires =
              fresh_int_list used_wires out_sum_size
            in
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
              ( {
                  name = "mixed_err " ^ circ.name;
                  in_regs;
                  prep_reg =
                    int_list_union (signal_bit :: circ.prep_reg) fresh_prep;
                  out_regs =
                    [
                      List.hd circ.out_regs;
                      signal_bit :: List.flatten (List.tl circ.out_regs);
                    ];
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
                                (List.flatten (List.tl circ.out_regs))
                                fresh_prep
                           @& Controlled
                                ( circ.flag_reg,
                                  list_constant false
                                    (List.length circ.flag_reg),
                                  gate_paulix signal_bit
                                  @& gate_swap_regs
                                       (List.flatten (List.tl circ.out_regs))
                                       fresh_prep )
                       end;
                },
                used_wires )
        end;
    }

(** Pure error handling circuit: given a circuit specification corresponding to
    a Kraus operator with output space {m \mathcal{H}}, converts it into a
    circuit corresponding to a norm-preserving operator with output space
    {m \mathcal{H} \oplus \mathcal{H}_F} for some flag space {m \mathcal{H}_F}.
    This just sends the flag registers to the output and adds an additional
    signaling qubit. *)
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

(** Measures the flag register output by the first circuit, and if it is
    nonzero, replaces the output with that of the second circuit. *)
let circuit_classical_try_catch (cs0 : circuit_spec) (cs1 : circuit_spec) :
    circuit_spec =
  let g_size = List.hd cs0.in_sizes in
  let d0_size = List.hd (List.tl cs0.in_sizes) in
  let d1_size = List.hd (List.tl cs1.in_sizes) in
    if cs0.out_sizes <> cs1.out_sizes || List.hd cs1.in_sizes <> g_size then
      failwith "circuit_classical_try_catch: expected matching sizes";
    {
      in_sizes = [g_size; d0_size; d1_size];
      out_sizes = cs0.out_sizes;
      circ_fun =
        begin
          fun in_regs used_wires settings ->
            expect_sizes "circuit_classical_try_catch"
              [g_size; d0_size; d1_size] in_regs;
            let g_reg = List.hd in_regs in
            let d0_reg = List.hd (List.tl in_regs) in
            let d1_reg = List.hd (List.tl (List.tl in_regs)) in
            let circ0, used_wires0 =
              build_circuit cs0 [g_reg; d0_reg] used_wires {settings with reset_flag = false}
            in
            (* Output of circ0 will be reset and reused as prep wires for circ1. *)
            let circ1, used_wires1 =
              build_circuit cs1 [g_reg; d1_reg]
                (IntSet.diff used_wires (IntSet.of_list d0_reg))
                settings
            in
              ( {
                  name = "classical_try_catch";
                  in_regs;
                  prep_reg = int_list_union circ0.prep_reg circ1.prep_reg;
                  out_regs = circ0.out_regs;
                  flag_reg = List.flatten circ1.out_regs @ circ1.flag_reg;
                  garb_reg = int_list_union circ0.garb_reg circ1.garb_reg;
                  gate =
                    circ0.gate @& IfNonzero circ0.flag_reg
                    @& List.fold_left ( @& ) Identity
                         (List.map gate_reset_reg
                            (circ0.out_regs @ [circ0.flag_reg; circ0.garb_reg]))
                    @& circ1.gate
                    @& gate_permute
                         (List.flatten circ0.out_regs
                         @ List.flatten circ1.out_regs)
                         (List.flatten circ1.out_regs
                         @ List.flatten circ0.out_regs)
                    @& Endif;
                },
                IntSet.union used_wires0 used_wires1 )
        end;
    }

(** Relative phase circuit, using a given circuit specification to define a
    subspace of the given type - different phases are applied to states inside
    and outside the subspace. *)
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

(*** Circuit that always causes an error by creating a nonzero value in the
  flag register. *)
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

(** Circuit that discards the input by putting the input register into the
    garbage register. *)
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

(** Purification of a circuit - ensures that when the circuit is built, no
    garbage wires are reset or reused. *)
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

(** Circuit modifier that ensures that [settings.iso] is set to [true] when the
    circuit is instantiated. *)
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

(** Given a context [d] and a register [reg] containing something in the space
    associated to the context, splits the register into smaller registers
    mapped to each variable in the context. *)
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

(** Partitions a register [reg] corresponding to a context [d] into a part
    corresponding to those variables that are in a specified set [fv] and those
    that are not. *)
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

(** Creates a circuit that partitions a register corresponding to a context [d]
    into a part corresponding to those variables that are in a specified set
    [fv] and those that are not. These two new registers form the output
    registers of the circuit. *)
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

(** A circuit that merges two disjoint ({e this is important!}) contexts into
    one. Since the contexts are disjoint, this merge is simply the adjoint of
    the partition circuit. *)
let circuit_context_merge (d0 : context) (d1 : context) : circuit_spec =
  let d =
    match map_merge false d0 d1 with
    | SomeE d -> d
    | NoneE err -> failwith err
  in
  let fv0 = map_dom d0 in
    circuit_adjoint (circuit_context_partition d fv0)

(** Given a circuit [circ] and an intermediate representation command [ic], as
    well as an intermediate representation valuation [iv], applies the
    command's operator to the registers of the circuit, instantiating a new
    circuit component using the given [used_wires] and [settings]. Returns an
    updated circuit and valuation (which has the new variables corresponding to
    the output registers of the circuit). *)
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

(** Applies a list of intermediate representation commands, in series, to a
    circuit, updating the valuation each time. *)
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

(** Converts an intermediate representation operator to a circuit, calling the
    previously defined circuits for most cases and calling the above functions
    for the user-defined [IFunc] case. Also does additional simplification in
    the [IAdjoint] case to ensure that the circuits can take advantage of qubit
    recycling. *)
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
  | ICommute (t0, t1) -> circuit_commute t0 t1
  | IConditionalCommute (t0, t1, path) ->
      circuit_conditional_commute t0 t1 path
  | IDistrLeft (t, t0, t1) -> circuit_distr_left t t0 t1
  | IDistrRight (t0, t1, t) -> circuit_distr_right t0 t1 t
  | IMixedErr op ->
      circuit_mixed_error_handling (compile_inter_op_to_circuit op)
  | IPureErr op -> circuit_pure_error_handling (compile_inter_op_to_circuit op)
  | IAlwaysErr size -> circuit_always_error size
  | IDiscard t -> circuit_discard (type_size t)
  | IContextDiscard d -> circuit_discard (context_size d)
  | IPurify op' -> circuit_purify (compile_inter_op_to_circuit op')
  | IMarkAsIso (iso, un, op') ->
      if un && not iso then failwith "If un holds, iso must also hold";
      circuit_mark_as_iso iso (compile_inter_op_to_circuit op')
  | IContextPartition (d, fv) -> circuit_context_partition d fv
  | IContextMerge (d0, d1) -> circuit_context_merge d0 d1
  | IClassicalTryCatch (op0, op1) ->
      circuit_classical_try_catch
        (compile_inter_op_to_circuit op0)
        (compile_inter_op_to_circuit op1)
  | IAdjoint (IAdjoint op) -> compile_inter_op_to_circuit op
  | IAdjoint (ISequence (op0, op1)) ->
      compile_inter_op_to_circuit (ISequence (IAdjoint op1, IAdjoint op0))
  | IAdjoint (IDirsum (op0, op1)) ->
      compile_inter_op_to_circuit (IDirsum (IAdjoint op0, IAdjoint op1))
  | IAdjoint (IMarkAsIso (_, false, op')) ->
      compile_inter_op_to_circuit (IAdjoint op')
  | IAdjoint (IMarkAsIso (false, true, _)) ->
      failwith "If un holds, iso must also hold"
  | IAdjoint (IMarkAsIso (true, true, op')) ->
      circuit_mark_as_iso true (compile_inter_op_to_circuit (IAdjoint op'))
  | IAdjoint (IFunc (name, args, icl, ret)) -> begin
      let icl' =
        List.rev_map
          (fun (target, op', args) -> (args, IAdjoint op', target))
          icl
      in
        compile_inter_op_to_circuit (IFunc (name, ret, icl', args))
    end
  | IAdjoint op' -> circuit_adjoint (compile_inter_op_to_circuit op')
  | IFunc (name, args, icl, ret) -> begin
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
                    "IFunc %s: arg_names = %s, in_regs = %s, arg_sizes = %s, \
                     ret_sizes = %s\n"
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
                         "IFunc %s: Returned variables do not correspond to \
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
                        "IFunc %s:\nret_regs: %s\nflag_reg: %s\ngarb_reg: %s\n"
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

(** An operator corresponding to an expression normally takes two registers,
    corresponding to the classical and quantum context. This transformation
    combines these into one. *)
let make_op_take_one_reg (g : context) (d : context) (t : exprtype)
    (op : inter_op) =
  let gsize = context_size g in
  let tsize = type_size t in
    IAdjoint (IContextMerge (g, d)) @&& op @&& ISizePair (gsize, tsize)

(** Takes the direct sum of a list of operators along a given binary tree
    structure. *)
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

(** Takes the direct sum of an operator with itself along a given binary tree
    structure. *)
let dirsum_op_by_tree (tree : binary_tree) (op : inter_op) =
  dirsum_op_list tree (List.map (fun _ -> op) (range (tree_size tree)))

(** Takes the direct sum of a list of operators along a given binary tree
    structure, leveled to the given height [h], which must be at least the
    height of [tree]. This ensures that the result can be separated into an
    index register and a data register that do not overlap. *)
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

(** Takes the direct sum of a type with itself along a given binary tree
    structure. *)
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

(** Takes the direct sum of a type with itself along a given binary tree
    structure. *)
let dirsum_type_by_tree (tree : binary_tree) (t : exprtype) =
  dirsum_type_list tree (List.map (fun _ -> t) (range (tree_size tree)))

(** Converts a sum type to a binary tree structure, creating a leaf when the
    type is no longer a sum type or when the [stop] type is reached. *)
let rec binary_tree_of_type (stop : exprtype) (t : exprtype) : binary_tree =
  match t with
  | _ when t = stop -> Leaf
  | SumType (t0, t1) ->
      Node (binary_tree_of_type stop t0, binary_tree_of_type stop t1)
  | _ -> Leaf

(** Type obtained from right-distributing the type [t1] along the direct sum
    tree represented by [t0], which has leaves at the [stop] type. *)
let rec big_distr_right_type (stop : exprtype) (t0 : exprtype) (t1 : exprtype)
    : exprtype =
  match t0 with
  | _ when t0 = stop -> ProdType (t0, t1)
  | SumType (t00, t01) ->
      SumType
        (big_distr_right_type stop t00 t1, big_distr_right_type stop t01 t1)
  | _ -> ProdType (t0, t1)

(** Right distributy isomorphism of the type [t1] along the direct sum tree
    represented by [t0], which has leaves at the [stop] type. *)
let rec big_distr_right_op (stop : exprtype) (t0 : exprtype) (t1 : exprtype) :
    inter_op =
  match t0 with
  | _ when t0 = stop -> IIdentity (ProdType (t0, t1))
  | SumType (t00, t01) -> begin
      inter_func "big_distr_right"
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

(** Type obtained from left-distributing the type [t0] along the direct sum
    tree represented by [t1], which has leaves at the [stop] type. *)
let rec big_distr_left_type (stop : exprtype) (t0 : exprtype) (t1 : exprtype) :
    exprtype =
  match t1 with
  | _ when t1 = stop -> ProdType (t0, t1)
  | SumType (t10, t11) ->
      SumType (big_distr_left_type stop t0 t10, big_distr_left_type stop t0 t11)
  | _ -> ProdType (t0, t1)

(** Left distributy isomorphism of the type [t0] along the direct sum tree
    represented by [t1], which has leaves at the [stop] type. *)
let rec big_distr_left_op (stop : exprtype) (t0 : exprtype) (t1 : exprtype) :
    inter_op =
  match t1 with
  | _ when t1 = stop -> IIdentity (ProdType (t0, t1))
  | SumType (t10, t11) -> begin
      inter_func "big_distr_left"
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

(** Converts a context into a fake product type of the types in its bindings. *)
let fake_type_of_context (d : context) : exprtype =
  let types = List.map snd (StringMap.bindings d) in
    List.fold_right (fun x cur -> ProdType (x, cur)) types Qunit

(** Converts a context list into a direct sum along a given tree of the fake
    types corresponding to its elements. *)
let fake_type_of_context_list (tree : binary_tree) (l : context list) :
    exprtype =
  dirsum_type_list tree (List.map fake_type_of_context l)

(** Creates a type whose size is the given integer. *)
let rec fake_type_of_size (size : int) : exprtype =
  if size <= 0 then Qunit else ProdType (bit, fake_type_of_size (size - 1))

(** Context distributivity operator that distributes the context [d] over the
    direct sum of contexts in the list [l] with the tree structure [tree],
    ensuring that the contexts are properly merged. *)
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

(** Converts a valued tree of contexts into a direct sum along the tree of the
    fake types corresponding to its elements. *)
let rec fake_type_of_context_tree (tree : context valued_binary_tree) :
    exprtype =
  match tree with
  | ValuedLeaf d -> fake_type_of_context d
  | ValuedNode (tree0, tree1) ->
      SumType (fake_type_of_context_tree tree0, fake_type_of_context_tree tree1)

(** Converts a tree transformation, assumed to act on the given valued tree of
    contexts, into an intermediate representation operator that transforms the
    structure of the corresponding direct sum type accordingly. *)
let rec inter_op_of_tree_transformation (trans : tree_transformation)
    (tree : context valued_binary_tree) : inter_op =
  match tree with
  | ValuedLeaf d -> ISizeIdentity (context_size d)
  | ValuedNode (tree0, tree1) -> begin
      let t0 = fake_type_of_context_tree tree0 in
      let t1 = fake_type_of_context_tree tree1 in
        match trans with
        | TreeIdentity -> IIdentity (SumType (t0, t1))
        | TreeLeftRotation -> begin
            match tree1 with
            | ValuedNode (tree10, tree11) -> begin
                let t10 = fake_type_of_context_tree tree10 in
                let t11 = fake_type_of_context_tree tree11 in
                  IAdjoint (IAssoc (t0, t10, t11))
              end
            | _ -> failwith "Can't convert left rotation to inter_op"
          end
        | TreeRightRotation -> begin
            match tree0 with
            | ValuedNode (tree00, tree01) -> begin
                let t00 = fake_type_of_context_tree tree00 in
                let t01 = fake_type_of_context_tree tree01 in
                  IAssoc (t0, t00, t01)
              end
            | _ -> failwith "Can't convert right rotation to inter_op"
          end
        | TreeCommute -> ICommute (t0, t1)
        | TreeLeftApply trans' ->
            IDirsum (inter_op_of_tree_transformation trans' tree0, IIdentity t1)
        | TreeRightApply trans' ->
            IDirsum (IIdentity t0, inter_op_of_tree_transformation trans' tree1)
        | TreeBothApply (trans0, trans1) ->
            IDirsum
              ( inter_op_of_tree_transformation trans0 tree0,
                inter_op_of_tree_transformation trans1 tree1 )
        | TreeConditionalCommute path -> IConditionalCommute (t0, t1, path)
        | TreeSequence (trans0, trans1) ->
            inter_op_of_tree_transformation trans0 tree
            @&& inter_op_of_tree_transformation trans1
                  (transform_valued_tree trans0 tree)
    end

(** Tree leveling operator that acts on a register corresponding to the direct
    sum of spaces with sizes specified in [l], using the binary tree structure
    [tree]. The operator, in effect, flattens the bottom of the tree to the
    given height [h], which must be at least the height of [tree]. It extends
    the branches by adding extra left children, thus ensuring that the result
    can be separated into an index register and a data register that do not
    overlap. *)
let rec tree_level_op (tree : binary_tree) (h : int) (l : int list) : inter_op
    =
  match (tree, l) with
  | Leaf, [s] -> begin
      inter_func "tree_level"
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

(** Compilation of the spanning judgment, producing a circuit
    {m \llbracket \mathrm{spanning}(e_1, \dots, e_n) \rrbracket} such that when
    {m \mathrm{spanning}(e_1, \dots, e_n)} holds with tree structure
    {m \mathcal{R}}, then for all {m j}, for all
    {m \sigma_j \in \mathbb{V}(\Gamma_j)}, we have that
    {m \llbracket \mathrm{spanning}(e_1, \dots, e_n) \rrbracket \llbracket e_j \rrbracket |\sigma_j\rangle = \mathrm{inj}_j^{\mathcal{R}}|\sigma_j\rangle }.
    This means that
    {m \llbracket \mathrm{spanning}(e_1, \dots, e_n) \rrbracket} corresponds to
    an operator
    {m \mathcal{H}(T) \rarr \bigoplus_{j=1}^n \mathcal{H}(\Gamma_j)} where the
    direct sum is along the tree structure {m \mathcal{R}}. This function,
    given the spanning proof structure from the type checker, outputs an
    intermediate representation operator that does this transformation, as well
    as the corresponding binary tree and context list of the {m \Gamma_j}. *)
let rec compile_spanning_to_inter_op (sp : spanning_proof) :
    inter_op * binary_tree * context list =
  match sp with
  | SVoid -> (IIdentity Void, Leaf, [])
  | SUnit -> (IIdentity Qunit, Leaf, [StringMap.empty])
  | SUnApp (f, sp') -> begin
      let op, tree, gl = compile_spanning_to_inter_op sp' in
        (IAdjoint (compile_pure_prog_to_inter_op f) @&& op, tree, gl)
    end
  | SVar (x, t) -> (IIdentity t, Leaf, [StringMap.singleton x t])
  | SSum (sp0, sp1) -> begin
      let sp0_op, tree0, gl0 = compile_spanning_to_inter_op sp0 in
      let sp1_op, tree1, gl1 = compile_spanning_to_inter_op sp1 in
        (IDirsum (sp0_op, sp1_op), Node (tree0, tree1), gl0 @ gl1)
    end
  | SPair (t0, t1, sp0, l) -> begin
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
                    inter_func "SPair_final_merge"
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
        if gl0_max_size = 0 then begin
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
            ( inter_func "SPair"
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
        else
          ( inter_func "SPair"
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

(** Orthogonality selection operator, which, in effect, removes certain leaves
    from a binary tree according to [selection]. This returns the operator
    which transforms the direct sum structure and sends unselected subspaces
    into the error space, as well as the updated tree. *)
and ortho_select_op (t : exprtype) (tree : binary_tree) (selection : bool list)
    : inter_op * binary_tree =
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

(** Compiles the orthogonality judgment into an intermediate representation
    operator, combining the compiled spanning operator and the orthogonality
    selection operator. *)
and compile_ortho_to_inter_op (orp : ortho_proof) :
    inter_op * binary_tree * context list =
  let sp, selection = orp in
  let span_op, span_tree, gl = compile_spanning_to_inter_op sp in
  let select_op, ortho_tree =
    ortho_select_op
      (dirsum_type_list span_tree (List.map fake_type_of_context gl))
      span_tree selection
  in
    ( span_op @&& select_op,
      ortho_tree,
      List.map fst (List.filter snd (List.combine gl selection)) )

(** Compiles the erasure judgment for a single variable into an intermediate
    representation operator. *)
and compile_single_erasure_to_inter_op (x : string) (xtype : exprtype)
    (erp : erasure_proof) =
  let xsize = type_size xtype in
    match erp with
    | EVar t -> begin
        let tsize = type_size t in
          inter_func "EVar"
            [("x", xsize); ("t", tsize)]
            [inter_letapp ["res"] (IAdjoint (IShare t)) ["t"; "x"]]
            [("res", tsize)]
      end
    | EPair0 (t0, t1, erp') -> begin
        let tsize = type_size (ProdType (t0, t1)) in
        let op' = compile_single_erasure_to_inter_op x xtype erp' in
          inter_func "EPair0"
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
          inter_func "EPair1"
            [("x", xsize); ("t", tsize)]
            [
              inter_letapp ["t0"; "t1"] (IAdjoint (IPair (t0, t1))) ["t"];
              inter_letapp ["t1"] op' ["x"; "t1"];
              inter_letapp ["res"] (IPair (t0, t1)) ["t0"; "t1"];
            ]
            [("res", tsize)]
      end

(** Compiles the erasure judgment into an intermediate representation operator. *)
and compile_erasure_to_inter_op (d : context) (t : exprtype)
    (l : (string * erasure_proof) list) : inter_op =
  let dsize = context_size d in
  let tsize = type_size t in
    match l with
    | [] -> begin
        inter_func "erasure_empty"
          [("d", dsize); ("t", tsize)]
          [inter_letapp [] (IAdjoint IEmpty) ["d"]]
          [("t", tsize)]
      end
    | (x, erp) :: l' -> begin
        let xtype = StringMap.find x d in
        let rest_d = StringMap.remove x d in
        let rest_op = compile_erasure_to_inter_op rest_d t l' in
        let cur_op = compile_single_erasure_to_inter_op x xtype erp in
          inter_func "erasure"
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

(** Compiles a pure expression into the intermediate representation. This
    creates a circuit that takes in two registers, corresponding to the
    classical and quantum contexts. *)
and compile_pure_expr_to_inter_op (tp : pure_expr_typing_proof) : inter_op =
  let t = type_of_pure_expr_proof tp in
  let g_whole, d_whole = context_of_pure_expr_proof tp in
  let gsize = context_size g_whole in
  let dsize = context_size d_whole in
  let tsize = type_size t in
    match tp with
    | TUnit _ -> begin
        inter_func_marked "TUnit" true true
          [("g", gsize); ("d", dsize)]
          []
          [("g", gsize); ("d", tsize)]
      end
    | TCvar { t; g; x } -> begin
        let xreg, grest = map_partition g (StringSet.singleton x) in
          inter_func_marked "TCvar" true false
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
    | TQvar { x; _ } -> begin
        inter_func_marked ("TQvar " ^ x) true true
          [("g", gsize); ("d", dsize)]
          []
          [("g", gsize); ("d", tsize)]
      end
    | TPurePair { t0; t1; d; d0; d1; e0; e1; iso; un; _ } -> begin
        let op0 = compile_pure_expr_to_inter_op e0 in
        let op1 = compile_pure_expr_to_inter_op e1 in
          inter_func_marked "TPurePair" iso un
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
    | TCtrl { t1; g; d; d'; e; l; orp; erp; iso; un; _ } -> begin
        let n = List.length l in
        let gdd' = map_merge_noopt false g d_whole in
          if n = 0 then
            inter_func_marked "TCtrl_Void" iso un
              [("g", gsize); ("dd*", dsize)]
              [
                inter_comment "Starting TCtrl";
                inter_letapp [] (IAlwaysErr dsize) ["dd*"];
                inter_letapp ["res"] IEmpty [];
                inter_comment "Finished TCtrl";
              ]
              [("g", gsize); ("res", 0)]
          else
            let e_op = compile_mixed_expr_to_inter_op e false in
            let gjs = List.map fst3 l in
            let ej's = List.map trd3 l in
            let ortho_op, ortho_tree, _ = compile_ortho_to_inter_op orp in
            let ej'_sum_op =
              dirsum_op_list ortho_tree
                (List.map
                   (fun (ej', gj) ->
                     make_op_take_one_reg
                       (map_merge_noopt false g gj)
                       d_whole t1
                       (compile_pure_expr_to_inter_op ej'))
                   (List.combine ej's gjs))
            in
            let erase_op =
              compile_erasure_to_inter_op d t1 (StringMap.bindings erp)
            in
            let fake_context_sumtype =
              fake_type_of_context_list ortho_tree
                (List.map (fun gj -> map_merge_noopt false g gj) gjs)
            in
              inter_func_marked "TCtrl" iso un
                [("g", gsize); ("dd*", dsize)]
                [
                  inter_comment "Starting TCtrl";
                  inter_letapp ["d"; "d*"]
                    (IContextPartition (d_whole, map_dom d))
                    ["dd*"];
                  inter_letapp ["d"; "d0"] (IContextShare d) ["d"];
                  inter_letapp ["g"; "t0"; "garb"] (IPurify e_op) ["g"; "d0"];
                  inter_letapp ["sum(gj)"] ortho_op ["t0"];
                  inter_letapp ["dd*"] (IContextMerge (d, d')) ["d"; "d*"];
                  inter_letapp ["gdd*"]
                    (IContextMerge (g, d_whole))
                    ["g"; "dd*"];
                  inter_letapp ["sum(ggjdd*)"]
                    (big_context_distr_op ortho_tree gjs gdd')
                    ["sum(gj)"; "gdd*"];
                  inter_letapp ["sum(ggj*t1)"] ej'_sum_op ["sum(ggjdd*)"];
                  inter_letapp ["sum(ggj)*t1"]
                    (IAdjoint
                       (big_distr_right_op Qunit fake_context_sumtype t1))
                    ["sum(ggj*t1)"];
                  inter_letapp ["sum(ggj)"; "t1"]
                    (IAdjoint (IPair (fake_context_sumtype, t1)))
                    ["sum(ggj)*t1"];
                  inter_letapp ["sum(gj)"; "g"]
                    (IAdjoint (big_context_distr_op ortho_tree gjs g))
                    ["sum(ggj)"];
                  inter_letapp ["t0"] (IAdjoint ortho_op) ["sum(gj)"];
                  inter_letapp ["g"; "d0"] (IAdjoint (IPurify e_op))
                    ["g"; "t0"; "garb"];
                  inter_letapp ["t1"] erase_op ["d0"; "t1"];
                  inter_comment "Finished TCtrl";
                ]
                [("g", gsize); ("t1", tsize)]
      end
    | TPureApp { f; e; iso; un; _ } -> begin
        let e_op = compile_pure_expr_to_inter_op e in
        let f_op = compile_pure_prog_to_inter_op f in
          inter_func_marked "TPureApp" iso un
            [("g", gsize); ("d", dsize)]
            [
              inter_comment "Starting TPureApp";
              inter_letapp ["g"; "t*"] e_op ["g"; "d"];
              inter_letapp ["res"] f_op ["t*"];
              inter_comment "Finished TPureApp";
            ]
            [("g", gsize); ("res", tsize)]
      end

(** Compiles a mixed expression into the intermediate representation. This
    creates a circuit that takes in one register, corresponding to the quantum
    context. *)
and compile_mixed_expr_to_inter_op (tp : mixed_expr_typing_proof)
    (classical : bool) : inter_op =
  let t = type_of_mixed_expr_proof tp in
  let g_whole, d_whole = context_of_mixed_expr_proof tp in
  let gsize = context_size g_whole in
  let dsize = context_size d_whole in
  let tsize = type_size t in
    match tp with
    | TMix tp' -> begin
        let tp'_op = compile_pure_expr_to_inter_op tp' in
          inter_func "TMix"
            [("g", gsize); ("d", dsize)]
            [
              inter_comment "Starting TMix";
              inter_letapp ["g"; "res"] tp'_op ["g"; "d"];
              inter_comment "Finished TMix";
            ]
            [("g", gsize); ("res", tsize)]
      end
    | TDiscard { d; d0; e; iso; _ } -> begin
        let e_op = compile_mixed_expr_to_inter_op e classical in
          inter_func_marked "TDiscard" iso false
            [("g", gsize); ("dd0", dsize)]
            [
              inter_comment "Starting TDiscard";
              inter_letapp ["d"; "d0"]
                (IAdjoint (IContextMerge (d, d0)))
                ["dd0"];
              inter_letapp [] (IContextDiscard d0) ["d0"];
              inter_letapp ["g"; "res"] e_op ["g"; "d"];
              inter_comment "Finished TDiscard";
            ]
            [("g", gsize); ("res", tsize)]
      end
    | TMixedPair { t0; t1; d; d0; d1; e0; e1; iso; _ } -> begin
        let op0 = compile_mixed_expr_to_inter_op e0 classical in
        let op1 = compile_mixed_expr_to_inter_op e1 classical in
          inter_func_marked "TMixedPair" iso false
            [("g", gsize); ("dd0d1", dsize)]
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
              inter_letapp ["g"; "t0"] op0 ["g"; "dd0"];
              inter_letapp ["g"; "t1"] op1 ["g"; "d*d1"];
              inter_letapp ["res"] (IPair (t0, t1)) ["t0"; "t1"];
              inter_comment "Finished TMixedPair";
            ]
            [("g", gsize); ("res", tsize)]
      end
    | TMatch { t1; g; d; d0; d1; e; l; orp; iso; _ } -> begin
        let n = List.length l in
          if n = 0 then
            inter_func_marked "TCtrl_Void" iso false
              [("g", gsize); ("dd*", dsize)]
              [
                inter_comment "Starting TMatch";
                inter_letapp [] (IAlwaysErr dsize) ["dd*"];
                inter_letapp ["res"] IEmpty [];
                inter_comment "Finished TMatch";
              ]
              [("g", gsize); ("res", 0)]
          else
            let e_op = compile_mixed_expr_to_inter_op e false in
            let gjs = List.map fst3 l in
            let ej's = List.map trd3 l in
            let ortho_op, ortho_tree, _ = compile_ortho_to_inter_op orp in
            let ej'_ops =
              List.map (fun x -> compile_mixed_expr_to_inter_op x false) ej's
            in
            let garb_sizes =
              List.map
                begin
                  (* This is somewhat of a hack, breaking down the abstraction levels
                     to obtain the garbage register sizes. It also relies on the fact
                     that whether or not reset_flag and iso are set have no effect
                     on the garbage register size. *)
                  fun ej'_op ->
                    let ej'_cs =
                      compile_inter_op_to_circuit (IPurify ej'_op)
                    in
                    let in_regs, used_wires =
                      fresh_int_lists IntSet.empty ej'_cs.in_sizes
                    in
                    let ej'_circ, _ =
                      build_circuit ej'_cs in_regs used_wires
                        { reset_flag = false; reset_garb = false; iso = false }
                    in
                      List.length (List.hd (List.rev ej'_circ.out_regs))
                end
                ej'_ops
            in
            let ej'_sum_op =
              dirsum_op_list ortho_tree
                (List.map
                   (fun ((ej'_op, garb_size), gj) ->
                     IPurify
                       (make_op_take_one_reg
                          (map_merge_noopt false g gj)
                          (map_merge_noopt false d d1)
                          t1 ej'_op)
                     @&& ISizePair
                           ( context_size g + context_size gj + type_size t1,
                             garb_size ))
                   (List.combine (List.combine ej'_ops garb_sizes) gjs))
            in
            let final_rearrange_op =
              dirsum_op_list ortho_tree
                (List.map
                   begin
                     fun (gj, garb_size) ->
                       let total_size =
                         context_size g + context_size gj + type_size t1
                         + garb_size
                       in
                         inter_func "final_rearrange_op"
                           [("ggj,t1,hj", total_size)]
                           [
                             inter_letapp ["ggj"; "t1,hj"]
                               (IAdjoint
                                  (ISizePair
                                     ( context_size g + context_size gj,
                                       type_size t1 + garb_size )))
                               ["ggj,t1,hj"];
                             inter_letapp ["g"; "gj"]
                               (IAdjoint (IContextMerge (g, gj)))
                               ["ggj"];
                             inter_letapp ["t1"; "hj"]
                               (IAdjoint (ISizePair (type_size t1, garb_size)))
                               ["t1,hj"];
                             inter_letapp ["g,t1"]
                               (ISizePair (context_size g, type_size t1))
                               ["g"; "t1"];
                             inter_letapp ["gj,hj"]
                               (ISizePair (context_size gj, garb_size))
                               ["gj"; "hj"];
                             inter_letapp ["gj,hj,g,t1"]
                               (ISizePair
                                  ( context_size gj + garb_size,
                                    context_size g + type_size t1 ))
                               ["gj,hj"; "g,t1"];
                           ]
                           [("gj,hj,g,t1", total_size)]
                   end
                   (List.combine gjs garb_sizes))
            in
            let discarded_type =
              dirsum_type_list ortho_tree
                (List.map
                   (fun (gj, garb_size) ->
                     ProdType
                       (fake_type_of_context gj, fake_type_of_size garb_size))
                   (List.combine gjs garb_sizes))
            in
              inter_func_marked "TCtrl" iso false
                [("g", gsize); ("dd0d1", dsize)]
                [
                  inter_comment "Starting TMatch";
                  inter_letapp ["d"; "d0d1"]
                    (IContextPartition (d_whole, map_dom d))
                    ["dd0d1"];
                  inter_letapp ["d0"; "d1"]
                    (IAdjoint (IContextMerge (d0, d1)))
                    ["d0d1"];
                  inter_letapp ["d"; "d*"] (IContextShare d) ["d"];
                  inter_letapp ["dd0"] (IContextMerge (d, d0)) ["d"; "d0"];
                  inter_letapp ["dd1"] (IContextMerge (d, d1)) ["d*"; "d1"];
                  inter_letapp ["g"; "t0"] e_op ["g"; "dd0"];
                  inter_letapp ["sum(gj)"] ortho_op ["t0"];
                  inter_letapp ["gdd1"]
                    (IContextMerge (g, map_merge_noopt false d d1))
                    ["g"; "dd1"];
                  inter_letapp ["sum(ggjdd1)"]
                    (big_context_distr_op ortho_tree gjs
                       (map_merge_noopt false g (map_merge_noopt false d d1)))
                    ["sum(gj)"; "gdd1"];
                  inter_letapp ["sum(ggj,t1,hj)"] ej'_sum_op ["sum(ggjdd1)"];
                  inter_letapp ["sum(gj,hj,g,t1)"] final_rearrange_op
                    ["sum(ggj,t1,hj)"];
                  inter_letapp ["garb,g,t1"]
                    (IAdjoint
                       (big_distr_right_op Qunit discarded_type
                          (ProdType (fake_type_of_context g, t1))))
                    ["sum(gj,hj,g,t1)"];
                  inter_letapp ["garb"; "g,t1"]
                    (IAdjoint
                       (ISizePair
                          ( type_size discarded_type,
                            context_size g + type_size t1 )))
                    ["garb,g,t1"];
                  inter_letapp ["g"; "t1"]
                    (IAdjoint (ISizePair (context_size g, type_size t1)))
                    ["g,t1"];
                  inter_letapp [] (IDiscard discarded_type) ["garb"];
                  inter_comment "Finished TMatch";
                ]
                [("g", gsize); ("t1", tsize)]
      end
    | TTry { t; d0; e0; e1; iso; _ } -> begin
        let op0 = compile_mixed_expr_to_inter_op e0 classical in
        let op1 = compile_mixed_expr_to_inter_op e1 classical in
        let iso0 = is_iso_mixed_expr_proof e0 in
        let iso1 = is_iso_mixed_expr_proof e1 in
          if iso0 then
            op0
          else if classical then
            inter_func_marked "TTry" iso false
              [("g", gsize); ("d", dsize)]
              [
                inter_comment "Starting TTry";
                inter_letapp ["d0"; "d1"]
                  (IContextPartition (d_whole, map_dom d0))
                  ["d"];
                inter_letapp ["g"; "t"]
                  (IClassicalTryCatch (op0, op1))
                  ["g"; "d0"; "d1"];
                inter_comment "Finished TTry";
              ]
              [("g", gsize); ("t", tsize)]
          else if iso1 then
            inter_func_marked "TTry" iso false
              [("g", gsize); ("d", dsize)]
              [
                inter_comment "Starting TTry";
                inter_letapp ["d0"; "d1"]
                  (IContextPartition (d_whole, map_dom d0))
                  ["d"];
                inter_letapp ["g"; "t+c"] (IMixedErr op0) ["g"; "d0"];
                inter_letapp ["g"; "t"] op1 ["g"; "d1"];
                inter_letapp ["t*t+t"] (IDistrRight (t, Qunit, t)) ["t+c"; "t"];
                inter_letapp ["t"; "t+c"]
                  (IAdjoint (IDistrLeft (t, t, Qunit)))
                  ["t*t+t"];
                inter_letapp [] (IDiscard (SumType (t, Qunit))) ["t+c"];
                inter_comment "Finished TTry";
              ]
              [("g", gsize); ("t", tsize)]
          else
            inter_func_marked "TTry" iso false
              [("g", gsize); ("d", dsize)]
              [
                inter_comment "Starting TTry";
                inter_letapp ["d0"; "d1"]
                  (IContextPartition (d_whole, map_dom d0))
                  ["d"];
                inter_letapp ["g"; "t+c0"] (IMixedErr op0) ["g"; "d0"];
                inter_letapp ["g"; "t+c1"] (IMixedErr op1) ["g"; "d1"];
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
              [("g", gsize); ("t", tsize)]
      end
    | TMixedApp { f; e; iso; _ } -> begin
        let e_op = compile_mixed_expr_to_inter_op e classical in
        let f_op = compile_mixed_prog_to_inter_op f classical in
          inter_func_marked "TMixedApp" iso false
            [("g", gsize); ("d", dsize)]
            [
              inter_comment "Starting TMixedApp";
              inter_letapp ["g"; "t*"] e_op ["g"; "d"];
              inter_letapp ["res"] f_op ["t*"];
              inter_comment "Finished TMixedApp";
            ]
            [("g", gsize); ("res", tsize)]
      end

(** Compiles a pure program into the intermediate representation. *)
and compile_pure_prog_to_inter_op (tp : pure_prog_typing_proof) : inter_op =
  match tp with
  | TGate (theta, phi, lambda) -> IU3 (theta, phi, lambda)
  | TLeft (t0, t1) -> IMarkAsIso (true, false, ILeft (t0, t1))
  | TRight (t0, t1) -> IMarkAsIso (true, false, IRight (t0, t1))
  | TPureAbs { t; t'; e; e'; iso; un; _ } -> begin
      let e_op = compile_pure_expr_to_inter_op e in
      let e'_op = compile_pure_expr_to_inter_op e' in
        inter_func_marked "TPureAbs" iso un
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
  | TRphase { t; e; r0; r1; iso; un; _ } -> begin
      let _, d = context_of_pure_expr_proof e in
      let e_op = compile_pure_expr_to_inter_op e in
      let e_op_no_g = make_op_take_one_reg StringMap.empty d t e_op in
        IMarkAsIso (iso, un, IRphase (t, e_op_no_g, r0, r1))
    end
  | TPmatch { orp0; orp1; perm0; perm1; iso; un; _ } -> begin
      let ortho_op0, tree0, djs0 = compile_ortho_to_inter_op orp0 in
      let ortho_op1, tree1, _ = compile_ortho_to_inter_op orp1 in
      let djs0 =
        List.map fst
          (List.sort (fun (_, a) (_, b) -> a - b) (List.combine djs0 perm0))
      in
      let valued_tree0 = assign_values_to_tree tree0 perm0 in
      let valued_tree1 = assign_values_to_tree tree1 perm1 in
      let trans = reshape_valued_tree valued_tree0 valued_tree1 in
      let reshape_op =
        inter_op_of_tree_transformation trans
          (assign_values_to_tree tree0 djs0)
      in
        IMarkAsIso (iso, un, ortho_op0 @&& reshape_op @&& IAdjoint ortho_op1)
    end

(** Compiles a mixed program into the intermediate representation. *)
and compile_mixed_prog_to_inter_op (tp : mixed_prog_typing_proof)
    (classical : bool) : inter_op =
  match tp with
  | TChannel tp' -> compile_pure_prog_to_inter_op tp'
  | TMixedAbs { t; t'; e; e'; iso; _ } -> begin
      let e_op = compile_pure_expr_to_inter_op e in
      let e'_op = compile_mixed_expr_to_inter_op e' classical in
        inter_func_marked "TMixedAbs" iso false
          [("t", type_size t)]
          [
            inter_comment "Starting TMixedAbs";
            inter_letapp ["g"] IEmpty [];
            inter_letapp ["g"; "d"] (IAdjoint e_op) ["g"; "t"];
            inter_letapp ["g"; "res"] e'_op ["g"; "d"];
            inter_letapp [] (IAdjoint IEmpty) ["g"];
            inter_comment "Finished TMixedAbs";
          ]
          [("res", type_size t')]
    end

(** Main procedure for compiling a Qunity expression: into the intermediate
    representation, then compiling the intermediate representation to a
    circuit, then applying several postprocessing steps, and outputting the
    circuit's gate and additional relevant information. *)
let expr_compile (e : expr) : gate * int * int list =
  let tp = mixed_type_check_noopt e in
  let op = compile_mixed_expr_to_inter_op tp true in
  let cs = compile_inter_op_to_circuit op in
  let circ, _ =
    build_circuit cs [[]; []] IntSet.empty
      { reset_flag = true; reset_garb = true; iso = false }
  in
  let out_reg =
    match circ.out_regs with
    | [[]; out_reg] -> out_reg
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
