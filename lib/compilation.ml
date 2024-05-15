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

type inter_op =
  | IIdentity of exprtype (* a {T} -> a *)
  | ILeft of exprtype * exprtype (* a {T0} -> left{T0, T1} a *)
  | IRight of exprtype * exprtype (* a {T1} -> right{T0, T1} a *)
  | IPair of exprtype * exprtype (* a {T0}, b {T1} -> (a, b) {T0 * T1} *)
  | IShare of exprtype (* a {T} -> [a; a] *)
  | IContextShare of context
  | IAdjoint of inter_op
  | ISequence of inter_op * inter_op
  | ITensor of inter_op * inter_op
  | IDirsum of inter_op * inter_op
  | IDistr of
      exprtype * exprtype * exprtype (* T * (T0 + T1) -> T * T0 + T * T1 *)
  | IMixedErr of inter_op
  | IPureErr of inter_op
  | IPurify of inter_op
  | IContextPartition of
      context * StringSet.t (* context -> [things in set; things not in set] *)
  | IContextMerge of (context * context)
    (* merge contexts assumed to be disjoint *)
  | ILambda of (string list * string list * inter_expr)
(* Non-erased arg names, erased arg names, body *)

and inter_expr =
  | INull
  | IVar of string
  | ILetApply of string list * inter_op * inter_expr list * inter_expr list
    (* names to store outputs, operator, non-erased args, erased args *)
  | IApply of inter_op * inter_expr list * inter_expr list
  | IList of inter_expr list
  | IExprSeq of inter_expr * inter_expr

let inter_lambda (nearglist : string list) (earglist : string list)
    (body : inter_expr) : inter_op =
  ILambda (nearglist, earglist, body)

let inter_letapp (target : string list) (op : inter_op)
    (neargs : inter_expr list) (eargs : inter_expr list) =
  ILetApply (target, op, neargs, eargs)

let inter_app (op : inter_op) (neargs : inter_expr list)
    (eargs : inter_expr list) =
  IApply (op, neargs, eargs)

let rec inter_expr_seq (l : inter_expr list) : inter_expr =
  match l with
  | [] -> INull
  | [e] -> e
  | [e0; e1] -> IExprSeq (e0, e1)
  | h :: t -> IExprSeq (h, inter_expr_seq t)

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

let unitary_cnot (b0 : int) (b1 : int) : unitary =
  Controlled ([b0], [true], PauliX b1)

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

let circ_apply (cur_circ : circuit) (cs : circuit_spec)
    (in_regs : int list list) : circuit * int list list * IntSet.t =
  let rest_regs =
    List.map
      (fun x -> int_list_diff x (List.flatten in_regs))
      cur_circ.out_regs
  in
  let circ = cs.circ_fun in_regs cur_circ.used_wires in
    ( {
        prep_reg = cur_circ.prep_reg @ circ.prep_reg;
        out_regs = rest_regs @ circ.out_regs;
        flag_reg = cur_circ.flag_reg @ circ.flag_reg;
        garb_reg = cur_circ.garb_reg @ circ.garb_reg;
        used_wires = circ.used_wires;
        gate = cur_circ.gate @& circ.gate;
      },
      circ.out_regs,
      circ.used_wires )

let circuit_identity (sizes : int list) : circuit_spec =
  {
    in_sizes = sizes;
    out_sizes = sizes;
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

let circuit_indexed_share (sizes : int list) (i : int) : circuit_spec =
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

let context_reg_restriction (d : context) (reg : int list) (fv : StringSet.t) :
    int list =
  let reg_map = context_reg_split d reg in
  let restricted_reg_map =
    StringMap.filter (fun x _ -> StringSet.mem x fv) reg_map
  in
    List.fold_left
      (fun l (_, l') -> l @ l')
      []
      (StringMap.bindings restricted_reg_map)

let rec compile_pure_expr_to_inter_op (g : context) (d : context) (e : expr) :
    inter_op =
  let t =
    match pure_type_check g d e with
    | SomeE t -> t
    | NoneE err -> failwith err
  in
    match e with
    | Null -> IIdentity Qunit
    | Var x -> begin
        match StringMap.bindings d with
        | [] -> begin
            inter_lambda ["g"] []
              begin
                inter_expr_seq
                  [
                    inter_letapp ["x"; "rest"]
                      (IContextPartition (g, StringSet.singleton x))
                      [IVar "g"] [];
                    inter_app (IShare t) [IVar "x"] [];
                  ]
              end
          end
        | [(x', _)] when x' = x -> begin
            inter_lambda ["g"] ["d"]
              begin
                inter_app (IIdentity t) [] [IVar "d"]
              end
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
          inter_lambda ["g"] ["d"]
            begin
              inter_expr_seq
                [
                  inter_letapp ["d01"; "d_xor"]
                    (IContextPartition (d, fv01))
                    [] [IVar "d"];
                  inter_letapp ["d0"; "d1"]
                    (IContextPartition (d_xor, fv0))
                    [] [IVar "d_xor"];
                  inter_letapp ["d01*"] (IContextShare d01) [IVar "d01"] [];
                  inter_letapp ["d01_0"]
                    (IContextMerge (d01, d0))
                    [] [IVar "d01"; IVar "d0"];
                  inter_letapp ["d01*_1"]
                    (IContextMerge (d01, d1))
                    [] [IVar "d01*"; IVar "d1"];
                  inter_letapp ["t0"] op0 [IVar "g"] [IVar "d01_0"];
                  inter_letapp ["t1"] op1 [IVar "g"] [IVar "d01*_1"];
                  inter_app (IPair (t0, t1)) [] [IVar "t0"; IVar "t1"];
                ]
            end
      end
    | _ -> failwith "TODO"
