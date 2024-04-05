open Util

type matrix = { r : int; c : int; f : int -> int -> Complex.t }

let complex_sum : Complex.t list -> Complex.t =
  List.fold_left Complex.add Complex.zero

let rec complex_sum_to_n (n : int) (f : int -> Complex.t) =
  if n <= 0 then
    Complex.zero
  else
    Complex.add (f (n - 1)) (complex_sum_to_n (n - 1) f)

let mat_well_formed (m : matrix) : bool = m.r > 0 && m.c > 0

let mat_optimize (m : matrix) : matrix =
  let arr =
    Array.of_list
      (List.map
         (fun i -> Array.of_list (List.map (fun j -> m.f i j) (range m.c)))
         (range m.r))
  in
    { r = m.r; c = m.c; f = (fun i j -> arr.(i).(j)) }

let mat_plus (m1 : matrix) (m2 : matrix) : matrix =
  if m1.r <> m2.r || m1.c <> m2.c then
    invalid_arg
      (Printf.sprintf
         "Inconsistent matrix dimensions in matrix addition: %dx%d and %dx%d"
         m1.r m1.c m2.r m2.c)
  else
    { r = m1.r; c = m1.c; f = (fun i j -> Complex.add (m1.f i j) (m2.f i j)) }

let mat_transpose (m : matrix) : matrix =
  { r = m.c; c = m.r; f = (fun i j -> m.f j i) }

let mat_adjoint (m : matrix) : matrix =
  { r = m.c; c = m.r; f = (fun i j -> Complex.conj (m.f j i)) }

let mat_scalar_mul (z : Complex.t) (m : matrix) : matrix =
  { r = m.r; c = m.c; f = (fun i j -> Complex.mul z (m.f i j)) }

let mat_mul (m1 : matrix) (m2 : matrix) : matrix =
  if m1.c <> m2.r then
    invalid_arg
      (Printf.sprintf
         "Inconsistent matrix dimensions in matrix multiplication: %dx%d and \
          %dx%d"
         m1.r m1.c m2.r m2.c)
  else
    mat_optimize
      {
        r = m1.r;
        c = m2.c;
        f =
          (fun i j ->
            complex_sum_to_n m1.c (fun k -> Complex.mul (m1.f i k) (m2.f k j)));
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

let vec_dirsum (m1 : matrix) (m2 : matrix) : matrix =
  if m1.c <> 1 || m2.c <> 1 then
    failwith "Direct sum only applies to vectors"
  else
    {
      r = m1.r + m2.r;
      c = 1;
      f = (fun i _ -> if i < m1.r then m1.f i 0 else m2.f (i - m1.r) 0);
    }

let mat_column (m : matrix) (c : int) : matrix =
  { r = m.r; c = 1; f = (fun i j -> if j = 0 then m.f i c else Complex.zero) }

let mat_trace (m : matrix) : Complex.t =
  if m.r <> m.c then
    failwith "Matrix must be square"
  else
    complex_sum_to_n m.r (fun i -> m.f i i)

let mat_to_scalar (m : matrix) : Complex.t =
  if m.r <> 1 || m.c <> 1 then
    failwith (Printf.sprintf "Matrix must be 1x1, instead got %dx%d" m.r m.c)
  else
    mat_trace m

let mat_from_basis_action (c : int) (bfun : int -> matrix) =
  let r = (bfun 0).r in
    mat_optimize
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

let mat_from_u3 (theta : float) (phi : float) (lambda : float) : matrix =
  {
    r = 2;
    c = 2;
    f =
      (fun i j ->
        begin
          match (i, j) with
          | 0, 0 -> Complex.polar (Float.cos (theta /. 2.)) 0.
          | 0, 1 -> Complex.polar (-.Float.sin (theta /. 2.)) lambda
          | 1, 0 -> Complex.polar (Float.sin (theta /. 2.)) phi
          | 1, 1 -> Complex.polar (Float.cos (theta /. 2.)) (phi +. lambda)
          | _ -> Complex.zero
        end);
  }

let mat_zero (r : int) (c : int) = { r; c; f = (fun _ _ -> Complex.zero) }
let vec_zero (dim : int) = mat_zero dim 1

let mat_sum r c : matrix list -> matrix =
  List.fold_left mat_plus (mat_zero r c)

let mat_from_list (l : Complex.t list list) : matrix =
  let r = List.length l in
  let c = List.length (List.hd l) in
    { r; c; f = (fun i j -> List.nth (List.nth l i) j) }

let string_of_complex (z : Complex.t) =
  Printf.sprintf "%.3f%s%.3fi" z.re (if z.im >= 0. then "+" else "") z.im

let print_mat (m : matrix) =
  for i = 0 to m.r - 1 do
    for j = 0 to m.c - 1 do
      Printf.printf "%13s " (string_of_complex (m.f i j))
    done;
    Printf.printf "\n"
  done

let mat_approx_equal (m1 : matrix) (m2 : matrix) : bool =
  m1.r = m2.r && m1.c = m2.c
  && List.for_all
       (fun i ->
         List.for_all
           (fun j ->
             float_approx_equal
               (Complex.norm (Complex.sub (m1.f i j) (m2.f i j)))
               0.)
           (range m1.c))
       (range m1.r)

type superoperator = {
  dim_from : int;
  dim_to : int;
  (* to_row, to_col, from_row, from_col *)
  f : int -> int -> int -> int -> Complex.t;
}

let superop_optimize (super : superoperator) : superoperator =
  let arr =
    Array.of_list
      (List.map
         (fun i ->
           Array.of_list
             (List.map
                (fun j ->
                  Array.of_list
                    (List.map
                       (fun k ->
                         Array.of_list
                           (List.map
                              (fun l -> super.f i j k l)
                              (range super.dim_from)))
                       (range super.dim_from)))
                (range super.dim_to)))
         (range super.dim_to))
  in
    {
      dim_from = super.dim_from;
      dim_to = super.dim_to;
      f = (fun i j k l -> arr.(i).(j).(k).(l));
    }

let superop_plus (super1 : superoperator) (super2 : superoperator) :
    superoperator =
  if super1.dim_from <> super2.dim_from || super1.dim_to <> super2.dim_to then
    failwith "Inconsistent dimensions in superoperator addition"
  else
    {
      dim_from = super1.dim_from;
      dim_to = super1.dim_to;
      f = (fun i j k l -> Complex.add (super1.f i j k l) (super2.f i j k l));
    }

let superop_scalar_mul (z : Complex.t) (super : superoperator) : superoperator
    =
  {
    dim_from = super.dim_from;
    dim_to = super.dim_to;
    f = (fun i j k l -> Complex.mul z (super.f i j k l));
  }

let superop_apply (super : superoperator) (m : matrix) : matrix =
  if m.r <> super.dim_from || m.c <> super.dim_from then
    failwith "Inconsistent dimensions in superoperator application"
  else
    mat_optimize
      {
        r = super.dim_to;
        c = super.dim_to;
        f =
          (fun i j ->
            complex_sum_to_n super.dim_from (fun k ->
                complex_sum_to_n super.dim_from (fun l ->
                    Complex.mul (m.f k l) (super.f i j k l))));
      }

let superop_from_basis_action (dim_from : int) (bfun : int -> int -> matrix) :
    superoperator =
  let dim_to = (bfun 0 0).r in
    superop_optimize
      {
        dim_from;
        dim_to;
        f =
          (fun i j k l ->
            let bmat = bfun k l in
              if bmat.r <> dim_to || bmat.c <> dim_to then
                failwith "Invalid basis action"
              else
                bmat.f i j);
      }

let superop_on_basis (super : superoperator) (k : int) (l : int) : matrix =
  { r = super.dim_to; c = super.dim_to; f = (fun i j -> super.f i j k l) }
