# The Intermediate Language

This document describes the intermediate language used in the compilation process of compiling Qunity to low-level quantum circuits. This is an imperative language that allows registers to be manipulated as variables, and it contains primitives for several Kraus operators and norm non-decreasing superoperators defined in Appendix H.1 of Voichick et al. Because these are taken as primitives, one does not need to consider how to use prep/flag/garbage wires or ancillary qubits to implement these operators in terms of unitaries, because this will automatically be done when the language is converted to low-level circuits.

## Syntax

The syntax of the intermediate language is defined as follows:

```ocaml
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
```

### Expressions

`INull` represents an empty quantum register. `IVar` references some previously defined variable. `ILetApply` applies a given operator to a given list of expressions, and then stores the results in variables with provided names. `IApply` is the same, except it just evaluates to the result of applying the operation to the inputs. The difference between "erased" and "non-erased" arguments will be explained below. `IList` corresponds to a list of expressions. It is important that the output of an operator must always be an `IList`. `ISeq` describes a sequence of expressions. When evaluating the expression, a sequence will evaluate to the result of the last expression in the sequence - but that can depend on some variables defined in the previous steps.

### Operators

The operators correspond to the Kraus operators and trace non-increasing superoperators described in Appendix H.1. In addition to these, there is also `IContextPartition`, parameterized by a context and a set of free variables. Given a register corresponding to this context, this operator outputs a pair of registers corresponding to the subset of the context with variables contained in the set, and the rest of the context. In terms of actual quantum gates, this will be an identity, but this allows us to more easily construct the high-level circuits when Qunity's typing judgements rely on certain subsets of contexts. Finally, there is an `ILambda`, which corresponds to custom operators. An `ILambda` is what will be output when converting any Qunity expression or program to the intermediate language. When converting an expression with a pure typing judgement $\Gamma || \Delta \vdash e : T$, this operator will take in a register corresponding to the classical context $\Gamma$ and one corresponding to the quantum context $\Delta$ and output one corresponding to the type $T$. Really, the operator is $\mathcal{H}(\Gamma) \otimes \mathcal{H}(\Delta) \rightarrow \mathcal{H}(\Gamma) \otimes \mathcal{H}(T)$, but since the register corresponding to $\Gamma$ is not an "erased argument", as explained below, it is not considered part of the output. For a mixed typing judgement $\Delta \Vdash e : T$, this just becomes $\mathcal{H}(\Delta) \rightarrow \mathcal{H}(T)$, since there is no classical context. For pure and mixed programs ($T \rightsquigarrow T'$ and $T \Rrightarrow T'$), the operator is $\mathcal{H}(T) \rightarrow \mathcal{H}(T')$.

### Erased and Non-Erased Arguments

Suppose that we refer to some registers by variables, and then feed them into an operator, which outputs some new registers. In that case, the qubits in the output registers may contain the qubits from the input registers, as well as some new ancillary qubits, rearranged in an arbitrary way. This means that once we do this, we can no longer use the variable associated with the input register, and it must be discarded or erased. However, in some cases, we _can_ guarantee that the qubits of the input register will be untouched by the operator. This is the case with classical contexts - the registers associated with them store classical data, and are only used as controls for quantum gates, such as the share gate in T-CVAR. So, when describing an operator, we can designate some of its inputs as "non-erased", in which case the evaluation will not remove these variables from the context.


## Example

The following example implements the circuit for T-PUREPAIR using the intermediate language.
![T-PUREPAIR circuit](T-PUREPAIR.png)

```ocaml
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
```

Here, `inter_expr_seq`, `inter_letapp`, and `inter_app` are just aliases for `IExprSeq`, `ILetApply` and `IApply`. The first statement in the sequence partitions the input quantum context register into the part shared by both expressions - `"d01"`, or $\Delta$ in the diagram, and `"d_xor"` corresponding to the rest, which is used in exactly one of the Qunity expressions, corresponding to $\Delta_0 \cup \Delta_1$ in the diagram. The second statement partitions that into $\Delta_0$ and $\Delta_1$. The next statement shares the $\Delta$ register into a new register. Note that `"d01"` is considered a non-erased argument here, which is why it can still be used in the following statements. The next statement merges $\Delta$ and $\Delta_0$ to act as an input to the circuit for $e_0$, and the next statement does the same for $\Delta_1$ and $e_1$, but merging with the new copy of the $\Delta$ register coming from the share gate. Then, the compiled operators for $e_0$ and $e_1$ are applied to the corresponding arguments, taking `"g"`, which corresponds to the classical context $\Gamma$, as a non-erased argument. Finally, the two outputs are merged into a single register, producing an output in $\mathcal{H}(T_0 \otimes T_1)$. Note that everything here, except for the actual application of $e_0$ and $e_1$, has no actual quantum gates associated with it and just turns into the identity when compiled to the low-level representation.

