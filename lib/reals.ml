type real =
  | Pi
  | Euler
  | Const of int
  | Negate of real
  | Plus of (real * real)
  | Times of (real * real)
  | Div of (real * real)
  | Sin of real
  | Cos of real
  | Tan of real
  | Arcsin of real
  | Arccos of real
  | Arctan of real
  | Exp of real
  | Ln of real
  | Sqrt of real

let rec float_of_real (r : real) : float =
  match r with
  | Pi -> Float.pi
  | Euler -> Float.exp 1.
  | Const x -> float_of_int x
  | Negate r1 -> -1. *. float_of_real r1
  | Plus (r1, r2) -> float_of_real r1 +. float_of_real r2
  | Times (r1, r2) -> float_of_real r1 *. float_of_real r2
  | Div (r1, r2) -> float_of_real r1 /. float_of_real r2
  | Sin r1 -> Float.sin (float_of_real r1)
  | Cos r1 -> Float.cos (float_of_real r1)
  | Tan r1 -> Float.tan (float_of_real r1)
  | Arcsin r1 -> Float.asin (float_of_real r1)
  | Arccos r1 -> Float.acos (float_of_real r1)
  | Arctan r1 -> Float.atan (float_of_real r1)
  | Exp r1 -> Float.exp (float_of_real r1)
  | Ln r1 -> Float.log (float_of_real r1)
  | Sqrt r1 -> Float.sqrt (float_of_real r1)
