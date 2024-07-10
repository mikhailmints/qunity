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
  | Sqrt of real
  | Round of real

let rec string_of_real (r : real) : string =
  match r with
  | Pi -> "pi"
  | Euler -> "e"
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
  | Sqrt r0 -> Printf.sprintf "sqrt(%s)" (string_of_real r0)
  | Round r0 -> Printf.sprintf "round(%s)" (string_of_real r0)

let rec python_string_of_real (r : real) : string =
  match r with
  | Pi -> "np.pi"
  | Euler -> "np.e"
  | Const x -> string_of_int x
  | Negate r0 -> Printf.sprintf "-(%s)" (python_string_of_real r0)
  | Plus (r0, r1) ->
      Printf.sprintf "(%s) + (%s)" (python_string_of_real r0)
        (python_string_of_real r1)
  | Times (r0, r1) ->
      Printf.sprintf "(%s) * (%s)" (python_string_of_real r0)
        (python_string_of_real r1)
  | Div (r0, r1) ->
      Printf.sprintf "(%s) / (%s)" (python_string_of_real r0)
        (python_string_of_real r1)
  | Pow (r0, r1) ->
      Printf.sprintf "(%s) ** (%s)" (python_string_of_real r0)
        (python_string_of_real r1)
  | Mod (r0, r1) ->
      Printf.sprintf "(%s) %% (%s)" (python_string_of_real r0)
        (python_string_of_real r1)
  | Sin r0 -> Printf.sprintf "np.sin(%s)" (python_string_of_real r0)
  | Cos r0 -> Printf.sprintf "np.cos(%s)" (python_string_of_real r0)
  | Tan r0 -> Printf.sprintf "np.tan(%s)" (python_string_of_real r0)
  | Arcsin r0 -> Printf.sprintf "np.arcsin(%s)" (python_string_of_real r0)
  | Arccos r0 -> Printf.sprintf "np.arccos(%s)" (python_string_of_real r0)
  | Arctan r0 -> Printf.sprintf "np.arctan(%s)" (python_string_of_real r0)
  | Exp r0 -> Printf.sprintf "np.exp(%s)" (python_string_of_real r0)
  | Ln r0 -> Printf.sprintf "np.log(%s)" (python_string_of_real r0)
  | Sqrt r0 -> Printf.sprintf "np.sqrt(%s)" (python_string_of_real r0)
  | Round r0 -> Printf.sprintf "np.round(%s)" (string_of_real r0)

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
  | Sqrt r0 -> Printf.sprintf "Sqrt (%s)" (ocaml_string_of_real r0)
  | Round r0 -> Printf.sprintf "Round (%s)" (ocaml_string_of_real r0)

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
  | Sqrt r0 -> Float.sqrt (float_of_real r0)
  | Round r0 -> Float.round (float_of_real r0)

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
  | Round r0 -> begin
      match ratio_of_real r0 with
      | Some q0 -> Some (Big_int.int_of_big_int (round_ratio q0))
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
  | Round r0 -> begin
      match ratio_of_real r0 with
      | Some q0 -> Some (ratio_of_big_int (round_ratio q0))
      | None -> None
    end
  | _ -> None

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
