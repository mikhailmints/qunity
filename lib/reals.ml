open Ratio
open Util

type real =
  | Pi
  | Euler
  | Const of int
  | Negate of real
  | Plus of (real * real)
  | Times of (real * real)
  | Div of (real * real)
  | Pow of (real * real)
  | Mod of (real * real)
  | Sin of real
  | Cos of real
  | Tan of real
  | Arcsin of real
  | Arccos of real
  | Arctan of real
  | Exp of real
  | Ln of real
  | Log2 of real
  | Sqrt of real
  | Ceil of real
  | Floor of real

let rec float_of_real (r : real) : float =
  match r with
  | Pi -> Float.pi
  | Euler -> Float.exp 1.
  | Const x -> float_of_int x
  | Negate r0 -> -1. *. float_of_real r0
  | Plus (r0, r1) -> float_of_real r0 +. float_of_real r1
  | Times (r0, r1) -> float_of_real r0 *. float_of_real r1
  | Div (r0, r1) -> float_of_real r0 /. float_of_real r1
  | Pow (r0, r1) -> Float.pow (float_of_real r0) (float_of_real r1)
  | Mod (r0, r1) -> mod_float (float_of_real r0) (float_of_real r1)
  | Sin r0 -> Float.sin (float_of_real r0)
  | Cos r0 -> Float.cos (float_of_real r0)
  | Tan r0 -> Float.tan (float_of_real r0)
  | Arcsin r0 -> Float.asin (float_of_real r0)
  | Arccos r0 -> Float.acos (float_of_real r0)
  | Arctan r0 -> Float.atan (float_of_real r0)
  | Exp r0 -> Float.exp (float_of_real r0)
  | Ln r0 -> Float.log (float_of_real r0)
  | Log2 r0 -> Float.log2 (float_of_real r0)
  | Sqrt r0 -> Float.sqrt (float_of_real r0)
  | Ceil r0 -> Float.ceil (float_of_real r0)
  | Floor r0 -> Float.floor (float_of_real r0)

let rec int_of_real (r : real) : int option =
  match r with
  | Const x -> Some x
  | Negate r0 -> begin
      match int_of_real r0 with
      | Some n0 -> Some (-n0)
      | None -> None
    end
  | Plus (r0, r1) -> begin
      match (int_of_real r0, int_of_real r1) with
      | Some n0, Some n1 -> Some (n0 + n1)
      | _ -> None
    end
  | Times (r0, r1) -> begin
      match (int_of_real r0, int_of_real r1) with
      | Some n0, Some n1 -> Some (n0 * n1)
      | _ -> None
    end
  | Mod (r0, r1) -> begin
      match (int_of_real r0, int_of_real r1) with
      | Some n0, Some n1 -> Some (n0 mod n1)
      | _ -> None
    end
  | Ceil r0 -> begin
      match ratio_of_real r0 with
      | Some q0 -> Some (Big_int.int_of_big_int (ceiling_ratio q0))
      | None -> None
    end
  | Floor r0 -> begin
      match ratio_of_real r0 with
      | Some q0 -> Some (Big_int.int_of_big_int (floor_ratio q0))
      | None -> None
    end
  | _ -> None

and ratio_of_real (r : real) : ratio option =
  match r with
  | Const x -> Some (ratio_of_int x)
  | Negate r0 -> begin
      match ratio_of_real r0 with
      | Some q0 -> Some (minus_ratio q0)
      | None -> None
    end
  | Plus (r0, r1) -> begin
      match (ratio_of_real r0, ratio_of_real r1) with
      | Some q0, Some q1 -> Some (add_ratio q0 q1)
      | _ -> None
    end
  | Times (r0, r1) -> begin
      match (ratio_of_real r0, ratio_of_real r1) with
      | Some q0, Some q1 -> Some (mult_ratio q0 q1)
      | _ -> None
    end
  | Div (r0, r1) -> begin
      match (ratio_of_real r0, ratio_of_real r1) with
      | Some q0, Some q1 -> Some (div_ratio q0 q1)
      | _ -> None
    end
  | Mod (r0, r1) -> begin
      match (int_of_real r0, int_of_real r1) with
      | Some n0, Some n1 -> Some (ratio_of_int (n0 mod n1))
      | _ -> None
    end
  | Pow (r0, r1) -> begin
      match (ratio_of_real r0, int_of_real r1) with
      | Some q0, Some n1 -> Some (power_ratio_positive_int q0 n1)
      | _ -> None
    end
  | Ceil r0 -> begin
      match ratio_of_real r0 with
      | Some q0 -> Some (ratio_of_big_int (ceiling_ratio q0))
      | None -> None
    end
  | Floor r0 -> begin
      match ratio_of_real r0 with
      | Some q0 -> Some (ratio_of_big_int (floor_ratio q0))
      | None -> None
    end
  | _ -> None

(*
If the values are integers or rational numbers or if they have exactly
the same real representation, the equality comparison is exact. Otherwise, float
comparison is used, and values are considered equal if their float
representations are approximately equal.
*)
let real_equal (r0 : real) (r1 : real) =
  if r0 = r1 then
    true
  else
    match (ratio_of_real r0, ratio_of_real r1) with
    | Some q0, Some q1 -> eq_ratio q0 q1
    | _ -> float_approx_equal (float_of_real r0) (float_of_real r1)

let real_le (r0 : real) (r1 : real) =
  if real_equal r0 r1 then
    true
  else
    match (ratio_of_real r0, ratio_of_real r1) with
    | Some q0, Some q1 -> le_ratio q0 q1
    | _ -> float_of_real r0 <= float_of_real r1

let real_lt (r0 : real) (r1 : real) =
  if real_equal r0 r1 then
    false
  else
    match (ratio_of_real r0, ratio_of_real r1) with
    | Some q0, Some q1 -> lt_ratio q0 q1
    | _ -> float_of_real r0 < float_of_real r1

let real_ge (r0 : real) (r1 : real) =
  if real_equal r0 r1 then
    true
  else
    match (ratio_of_real r0, ratio_of_real r1) with
    | Some q0, Some q1 -> ge_ratio q0 q1
    | _ -> float_of_real r0 >= float_of_real r1

let real_gt (r0 : real) (r1 : real) =
  if real_equal r0 r1 then
    false
  else
    match (ratio_of_real r0, ratio_of_real r1) with
    | Some q0, Some q1 -> gt_ratio q0 q1
    | _ -> float_of_real r0 > float_of_real r1

(*
String representation in Qunity syntax.
*)
let rec string_of_real (r : real) : string =
  match r with
  | Pi -> "pi"
  | Euler -> "euler"
  | Const x -> string_of_int x
  | Negate r0 -> Printf.sprintf "-(%s)" (string_of_real r0)
  | Plus (r0, r1) ->
      Printf.sprintf "(%s) + (%s)" (string_of_real r0) (string_of_real r1)
  | Times (r0, r1) ->
      Printf.sprintf "(%s) * (%s)" (string_of_real r0) (string_of_real r1)
  | Div (r0, r1) ->
      Printf.sprintf "(%s) / (%s)" (string_of_real r0) (string_of_real r1)
  | Pow (r0, r1) ->
      Printf.sprintf "(%s) ^ (%s)" (string_of_real r0) (string_of_real r1)
  | Mod (r0, r1) ->
      Printf.sprintf "(%s) %% (%s)" (string_of_real r0) (string_of_real r1)
  | Sin r0 -> Printf.sprintf "sin(%s)" (string_of_real r0)
  | Cos r0 -> Printf.sprintf "cos(%s)" (string_of_real r0)
  | Tan r0 -> Printf.sprintf "tan(%s)" (string_of_real r0)
  | Arcsin r0 -> Printf.sprintf "arcsin(%s)" (string_of_real r0)
  | Arccos r0 -> Printf.sprintf "arccos(%s)" (string_of_real r0)
  | Arctan r0 -> Printf.sprintf "arctan(%s)" (string_of_real r0)
  | Exp r0 -> Printf.sprintf "exp(%s)" (string_of_real r0)
  | Ln r0 -> Printf.sprintf "ln(%s)" (string_of_real r0)
  | Log2 r0 -> Printf.sprintf "log2(%s)" (string_of_real r0)
  | Sqrt r0 -> Printf.sprintf "sqrt(%s)" (string_of_real r0)
  | Ceil r0 -> Printf.sprintf "ceil(%s)" (string_of_real r0)
  | Floor r0 -> Printf.sprintf "floor(%s)" (string_of_real r0)

(*
Ideally this should be used when outputting to QASM, but Qiskit's
qiskit-qasm3-import currently doesn't seem to support math functions.
*)
let rec qasm_string_of_real (r : real) : string =
  match r with
  | Pi -> "pi"
  | Euler -> "euler"
  | Const x -> string_of_int x
  | Negate r0 -> Printf.sprintf "-(%s)" (qasm_string_of_real r0)
  | Plus (r0, r1) ->
      Printf.sprintf "(%s) + (%s)" (qasm_string_of_real r0)
        (qasm_string_of_real r1)
  | Times (r0, r1) ->
      Printf.sprintf "(%s) * (%s)" (qasm_string_of_real r0)
        (qasm_string_of_real r1)
  | Div (r0, r1) ->
      Printf.sprintf "(%s) / (%s)" (qasm_string_of_real r0)
        (qasm_string_of_real r1)
  | Pow (r0, r1) ->
      Printf.sprintf "pow(%s, %s)" (qasm_string_of_real r0)
        (qasm_string_of_real r1)
  | Mod (r0, r1) ->
      Printf.sprintf "mod(%s, %s)" (qasm_string_of_real r0)
        (qasm_string_of_real r1)
  | Sin r0 -> Printf.sprintf "sin(%s)" (qasm_string_of_real r0)
  | Cos r0 -> Printf.sprintf "cos(%s)" (qasm_string_of_real r0)
  | Tan r0 -> Printf.sprintf "tan(%s)" (qasm_string_of_real r0)
  | Arcsin r0 -> Printf.sprintf "arcsin(%s)" (qasm_string_of_real r0)
  | Arccos r0 -> Printf.sprintf "arccos(%s)" (qasm_string_of_real r0)
  | Arctan r0 -> Printf.sprintf "arctan(%s)" (qasm_string_of_real r0)
  | Exp r0 -> Printf.sprintf "exp(%s)" (qasm_string_of_real r0)
  | Ln r0 -> Printf.sprintf "log(%s)" (qasm_string_of_real r0)
  | Log2 r0 -> Printf.sprintf "log(%s)/log(2)" (qasm_string_of_real r0)
  | Sqrt r0 -> Printf.sprintf "sqrt(%s)" (qasm_string_of_real r0)
  | Ceil r0 -> Printf.sprintf "ceiling(%s)" (qasm_string_of_real r0)
  | Floor r0 -> Printf.sprintf "floor(%s)" (qasm_string_of_real r0)

(*
Version that is compatible with qiskit-qasm3-import.
*)
let rec qasm_string_of_real_temp (r : real) : string =
  match r with
  | Pi -> "pi"
  | Euler -> "euler"
  | Const x -> string_of_int x
  | Negate r0 -> Printf.sprintf "-(%s)" (qasm_string_of_real_temp r0)
  | Plus (r0, r1) ->
      Printf.sprintf "(%s) + (%s)"
        (qasm_string_of_real_temp r0)
        (qasm_string_of_real_temp r1)
  | Times (r0, r1) ->
      Printf.sprintf "(%s) * (%s)"
        (qasm_string_of_real_temp r0)
        (qasm_string_of_real_temp r1)
  | Div (r0, r1) ->
      Printf.sprintf "(%s) / (%s)"
        (qasm_string_of_real_temp r0)
        (qasm_string_of_real_temp r1)
  | _ -> Printf.sprintf "%f" (float_of_real r)

let rec ocaml_string_of_real (r : real) : string =
  match r with
  | Pi -> "Pi"
  | Euler -> "Euler"
  | Const x -> Printf.sprintf "Const %s" (string_of_int x)
  | Negate r0 -> Printf.sprintf "Negate (%s)" (ocaml_string_of_real r0)
  | Plus (r0, r1) ->
      Printf.sprintf "Plus (%s, %s)" (ocaml_string_of_real r0)
        (ocaml_string_of_real r1)
  | Times (r0, r1) ->
      Printf.sprintf "Times (%s, %s)" (ocaml_string_of_real r0)
        (ocaml_string_of_real r1)
  | Div (r0, r1) ->
      Printf.sprintf "Div (%s, %s)" (ocaml_string_of_real r0)
        (ocaml_string_of_real r1)
  | Pow (r0, r1) ->
      Printf.sprintf "Pow (%s, %s)" (ocaml_string_of_real r0)
        (ocaml_string_of_real r1)
  | Mod (r0, r1) ->
      Printf.sprintf "Mod (%s, %s)" (ocaml_string_of_real r0)
        (ocaml_string_of_real r1)
  | Sin r0 -> Printf.sprintf "Sin (%s)" (ocaml_string_of_real r0)
  | Cos r0 -> Printf.sprintf "Cos (%s)" (ocaml_string_of_real r0)
  | Tan r0 -> Printf.sprintf "Tan (%s)" (ocaml_string_of_real r0)
  | Arcsin r0 -> Printf.sprintf "Arcsin (%s)" (ocaml_string_of_real r0)
  | Arccos r0 -> Printf.sprintf "Arccos (%s)" (ocaml_string_of_real r0)
  | Arctan r0 -> Printf.sprintf "Arctan (%s)" (ocaml_string_of_real r0)
  | Exp r0 -> Printf.sprintf "Exp (%s)" (ocaml_string_of_real r0)
  | Ln r0 -> Printf.sprintf "Ln (%s)" (ocaml_string_of_real r0)
  | Log2 r0 -> Printf.sprintf "Log2 (%s)" (ocaml_string_of_real r0)
  | Sqrt r0 -> Printf.sprintf "Sqrt (%s)" (ocaml_string_of_real r0)
  | Ceil r0 -> Printf.sprintf "Ceil (%s)" (ocaml_string_of_real r0)
  | Floor r0 -> Printf.sprintf "Floor (%s)" (ocaml_string_of_real r0)
