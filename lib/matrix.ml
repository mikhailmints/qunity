open Util

type matrix = { r : int; c : int; f : int -> int -> Complex.t }

let mat_well_formed (m : matrix) : bool = m.r > 0 && m.c > 0

let mat_plus (m1 : matrix) (m2 : matrix) : matrix =
  if m1.r <> m2.r || m1.c <> m2.c then
    invalid_arg "inconsistent matrix dimensions"
  else
    { r = m1.r; c = m1.c; f = (fun i j -> Complex.add (m1.f i j) (m2.f i j)) }

let mat_transpose (m : matrix) : matrix =
  { r = m.c; c = m.r; f = (fun i j -> m.f j i) }

let mat_adjoint (m : matrix) : matrix =
  { r = m.c; c = m.r; f = (fun i j -> Complex.conj (m.f j i)) }

let rec sum_to_n (n : int) (f : int -> Complex.t) =
  if n <= 0 then
    Complex.zero
  else
    Complex.add (f n) (sum_to_n (n - 1) f)

let mat_mul (m1 : matrix) (m2 : matrix) : matrix =
  if m1.c <> m2.r then
    invalid_arg "inconsistent matrix dimensions"
  else
    {
      r = m1.r;
      c = m2.c;
      f =
        (fun i j -> sum_to_n m1.c (fun k -> Complex.mul (m1.f i k) (m2.f k j)));
    }

let ( *@ ) = mat_mul
let mat_outer (m : matrix) = m *@ mat_adjoint m

let mat_tensor (m1 : matrix) (m2 : matrix) : matrix =
  {
    r = m1.r * m2.r;
    c = m1.c * m2.c;
    f =
      (fun i j ->
        Complex.mul
          (m1.f (i / m2.r) (j / m2.c))
          (m2.f (i mod m2.r) (j mod m2.c)));
  }

let mat_dirsum (m1 : matrix) (m2 : matrix) : matrix =
  {
    r = m1.r + m2.r;
    c = m1.c + m2.c;
    f = (fun i j -> if i < m1.r then m1.f i j else m2.f (i - m1.r) (j - m1.c));
  }

let mat_column (m : matrix) (c : int) : matrix =
  { r = m.r; c = 1; f = (fun i j -> if j = 0 then m.f i c else Complex.zero) }

let mat_from_basis_action (c : int) (bfun : int -> matrix) =
  let r = (bfun 0).r in
    {
      r;
      c;
      f =
        (fun i j ->
          let bvec = bfun j in
            if bvec.r <> r || bvec.c <> 1 then
              failwith "Invalid basis action"
            else
              bvec.f i 0);
    }

let mat_id (n : int) =
  if n <= 0 then
    invalid_arg "Matrix size must positive"
  else
    {
      r = n;
      c = n;
      f = (fun i j -> if i = j then Complex.one else Complex.zero);
    }

let complex_from_polar (r : float) (theta : float) : Complex.t =
  { re = r *. Float.cos theta; im = r *. Float.sin theta }

let mat_from_u3 (theta : float) (phi : float) (lambda : float) : matrix =
  {
    r = 2;
    c = 2;
    f =
      (fun i j ->
        begin
          match (i, j) with
          | 0, 0 -> { re = Float.cos (theta /. 2.); im = 0. }
          | 0, 1 -> complex_from_polar (-.Float.sin (theta /. 2.)) lambda
          | 1, 0 -> complex_from_polar (Float.sin (theta /. 2.)) phi
          | 1, 1 -> complex_from_polar (Float.cos (theta /. 2.)) (phi +. lambda)
          | _ -> Complex.zero
        end);
  }

let mat_zero (r : int) (c : int) = { r; c; f = (fun _ _ -> Complex.zero) }
let vec_zero (dim : int) = mat_zero dim 1

let complex_sum : Complex.t list -> Complex.t =
  List.fold_left Complex.add Complex.zero

let mat_sum r c : matrix list -> matrix =
  List.fold_left mat_plus (mat_zero r c)

type superoperator = {
  dim_from : int;
  dim_to : int;
  (* to_row, to_col, from_row, from_col *)
  f : int -> int -> int -> int -> Complex.t;
}

let superoperator_apply (super : superoperator) (m : matrix) : matrix =
  if m.r <> super.dim_from || m.c <> super.dim_from then
    failwith "Inconsistent dimensions in superoperator application"
  else
    {
      r = super.dim_to;
      c = super.dim_to;
      f =
        (fun i j ->
          complex_sum
            (List.map
               (fun k ->
                 complex_sum
                   (List.map
                      (fun l -> Complex.mul (m.f k l) (super.f i j k l))
                      (range super.dim_from)))
               (range super.dim_from)));
    }
