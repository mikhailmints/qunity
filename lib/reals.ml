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

