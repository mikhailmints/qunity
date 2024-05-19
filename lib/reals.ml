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

type realexpr =
  | XPi
  | XEuler
  | XConst of int
  | XVar of string
  | XNegate of realexpr
  | XPlus of (realexpr * realexpr)
  | XTimes of (realexpr * realexpr)
  | XDiv of (realexpr * realexpr)
  | XPow of (realexpr * realexpr)
  | XMod of (realexpr * realexpr)
  | XSin of realexpr
  | XCos of realexpr
  | XTan of realexpr
  | XArcsin of realexpr
  | XArccos of realexpr
  | XArctan of realexpr
  | XExp of realexpr
  | XLn of realexpr
  | XSqrt of realexpr
  | XRound of realexpr

let rec string_of_real (r : real) : string =
  match r with
  | Pi -> "pi"
  | Euler -> "e"
  | Const x -> string_of_int x
  | Negate r1 -> Printf.sprintf "-(%s)" (string_of_real r1)
  | Plus (r1, r2) ->
      Printf.sprintf "(%s) + (%s)" (string_of_real r1) (string_of_real r2)
  | Times (r1, r2) ->
      Printf.sprintf "(%s) * (%s)" (string_of_real r1) (string_of_real r2)
  | Div (r1, r2) ->
      Printf.sprintf "(%s) / (%s)" (string_of_real r1) (string_of_real r2)
  | Pow (r1, r2) ->
      Printf.sprintf "(%s) ^ (%s)" (string_of_real r1) (string_of_real r2)
  | Mod (r1, r2) ->
      Printf.sprintf "(%s) %% (%s)" (string_of_real r1) (string_of_real r2)
  | Sin r1 -> Printf.sprintf "sin(%s)" (string_of_real r1)
  | Cos r1 -> Printf.sprintf "cos(%s)" (string_of_real r1)
  | Tan r1 -> Printf.sprintf "tan(%s)" (string_of_real r1)
  | Arcsin r1 -> Printf.sprintf "arcsin(%s)" (string_of_real r1)
  | Arccos r1 -> Printf.sprintf "arccos(%s)" (string_of_real r1)
  | Arctan r1 -> Printf.sprintf "arctan(%s)" (string_of_real r1)
  | Exp r1 -> Printf.sprintf "exp(%s)" (string_of_real r1)
  | Ln r1 -> Printf.sprintf "ln(%s)" (string_of_real r1)
  | Sqrt r1 -> Printf.sprintf "sqrt(%s)" (string_of_real r1)
  | Round r1 -> Printf.sprintf "round(%s)" (string_of_real r1)

let rec python_string_of_real (r : real) : string =
  match r with
  | Pi -> "np.pi"
  | Euler -> "np.e"
  | Const x -> string_of_int x
  | Negate r1 -> Printf.sprintf "-(%s)" (python_string_of_real r1)
  | Plus (r1, r2) ->
      Printf.sprintf "(%s) + (%s)" (python_string_of_real r1)
        (python_string_of_real r2)
  | Times (r1, r2) ->
      Printf.sprintf "(%s) * (%s)" (python_string_of_real r1)
        (python_string_of_real r2)
  | Div (r1, r2) ->
      Printf.sprintf "(%s) / (%s)" (python_string_of_real r1)
        (python_string_of_real r2)
  | Pow (r1, r2) ->
      Printf.sprintf "(%s) ** (%s)" (python_string_of_real r1)
        (python_string_of_real r2)
  | Mod (r1, r2) ->
      Printf.sprintf "(%s) %% (%s)" (python_string_of_real r1)
        (python_string_of_real r2)
  | Sin r1 -> Printf.sprintf "np.sin(%s)" (python_string_of_real r1)
  | Cos r1 -> Printf.sprintf "np.cos(%s)" (python_string_of_real r1)
  | Tan r1 -> Printf.sprintf "np.tan(%s)" (python_string_of_real r1)
  | Arcsin r1 -> Printf.sprintf "np.arcsin(%s)" (python_string_of_real r1)
  | Arccos r1 -> Printf.sprintf "np.arccos(%s)" (python_string_of_real r1)
  | Arctan r1 -> Printf.sprintf "np.arctan(%s)" (python_string_of_real r1)
  | Exp r1 -> Printf.sprintf "np.exp(%s)" (python_string_of_real r1)
  | Ln r1 -> Printf.sprintf "np.log(%s)" (python_string_of_real r1)
  | Sqrt r1 -> Printf.sprintf "np.sqrt(%s)" (python_string_of_real r1)
  | Round r1 -> Printf.sprintf "np.round(%s)" (string_of_real r1)

let rec float_of_real (r : real) : float =
  match r with
  | Pi -> Float.pi
  | Euler -> Float.exp 1.
  | Const x -> float_of_int x
  | Negate r1 -> -1. *. float_of_real r1
  | Plus (r1, r2) -> float_of_real r1 +. float_of_real r2
  | Times (r1, r2) -> float_of_real r1 *. float_of_real r2
  | Div (r1, r2) -> float_of_real r1 /. float_of_real r2
  | Pow (r1, r2) -> Float.pow (float_of_real r1) (float_of_real r2)
  | Mod (r1, r2) -> mod_float (float_of_real r1) (float_of_real r2)
  | Sin r1 -> Float.sin (float_of_real r1)
  | Cos r1 -> Float.cos (float_of_real r1)
  | Tan r1 -> Float.tan (float_of_real r1)
  | Arcsin r1 -> Float.asin (float_of_real r1)
  | Arccos r1 -> Float.acos (float_of_real r1)
  | Arctan r1 -> Float.atan (float_of_real r1)
  | Exp r1 -> Float.exp (float_of_real r1)
  | Ln r1 -> Float.log (float_of_real r1)
  | Sqrt r1 -> Float.sqrt (float_of_real r1)
  | Round r1 -> Float.round (float_of_real r1)
