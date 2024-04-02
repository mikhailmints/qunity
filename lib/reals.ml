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

let rec string_of_real (r : real) : string =
  match r with
  | Pi -> "pi"
  | Euler -> "e"
  | Const x -> string_of_int x
  | Negate r1 -> "-" ^ (string_of_real r1)
  | Plus (r1, r2) -> Printf.sprintf "(%s) + (%s)" (string_of_real r1) (string_of_real r2)
  | Times (r1, r2) -> Printf.sprintf "(%s) * (%s)" (string_of_real r1) (string_of_real r2)
  | Div (r1, r2) -> Printf.sprintf "(%s) / (%s)" (string_of_real r1) (string_of_real r2)
  | Sin (r1) -> Printf.sprintf "sin(%s)" (string_of_real r1)
  | Cos (r1) -> Printf.sprintf "cos(%s)" (string_of_real r1)
  | Tan (r1) -> Printf.sprintf "tan(%s)" (string_of_real r1)
  | Arcsin (r1) -> Printf.sprintf "arcsin(%s)" (string_of_real r1)
  | Arccos (r1) -> Printf.sprintf "arccos(%s)" (string_of_real r1)
  | Arctan (r1) -> Printf.sprintf "arctan(%s)" (string_of_real r1)
  | Exp (r1) -> Printf.sprintf "exp(%s)" (string_of_real r1)
  | Ln (r1) -> Printf.sprintf "ln(%s)" (string_of_real r1)
  | Sqrt (r1) -> Printf.sprintf "sqrt(%s)" (string_of_real r1)

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
