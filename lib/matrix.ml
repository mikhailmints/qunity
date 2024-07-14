open Util

module Int2Map = Map.Make (struct
  type t = int * int

  let compare = Stdlib.compare
end)

module Int4Map = Map.Make (struct
  type t = int * int * int * int

  let compare = Stdlib.compare
end)

type matrix_contents =
  | Dense of (int -> int -> Complex.t)
  | Sparse of Complex.t Int2Map.t

type matrix = { r : int; c : int; ent : matrix_contents }

let complex_sum : Complex.t list -> Complex.t =
  List.fold_left Complex.add Complex.zero

let rec complex_sum_to_n (n : int) (f : int -> Complex.t) =
  if n <= 0 then
    Complex.zero
  else
    Complex.add (f (n - 1)) (complex_sum_to_n (n - 1) f)

let mat_is_sparse (m : matrix) : bool =
  match m.ent with
  | Dense _ -> false
  | Sparse _ -> true

let mat_entry (m : matrix) (i : int) (j : int) : Complex.t =
  match m.ent with
  | Dense f -> f i j
  | Sparse s -> begin
      match Int2Map.find_opt (i, j) s with
      | Some z -> z
      | None -> Complex.zero
    end

let mat_from_fun (r : int) (c : int) (f : int -> int -> Complex.t) : matrix =
  { r; c; ent = Dense f }

let mat_from_map (r : int) (c : int) (s : Complex.t Int2Map.t) : matrix =
  { r; c; ent = Sparse s }

let mat_evaluate_dense (m : matrix) : matrix =
  let r_range = range m.r in
  let c_range = range m.c in
  let arr =
    Array.of_list
      (List.map
         (fun i -> Array.of_list (List.map (fun j -> mat_entry m i j) c_range))
         r_range)
  in
    { r = m.r; c = m.c; ent = Dense (fun i j -> arr.(i).(j)) }

let mat_scalar_mul (z : Complex.t) (m : matrix) : matrix =
  match m.ent with
  | Dense f ->
      { r = m.r; c = m.c; ent = Dense (fun i j -> Complex.mul z (f i j)) }
  | Sparse s ->
      {
        r = m.r;
        c = m.c;
        ent = Sparse (Int2Map.map (fun z' -> Complex.mul z z') s);
      }

let rec mat_plus (m1 : matrix) (m2 : matrix) : matrix =
  if m1.r <> m2.r || m1.c <> m2.c then
    invalid_arg
      (Printf.sprintf
         "Inconsistent matrix dimensions in matrix addition: %dx%d and %dx%d"
         m1.r m1.c m2.r m2.c)
  else
    match (m1.ent, m2.ent) with
    | Dense f1, Dense f2 ->
        {
          r = m1.r;
          c = m1.c;
          ent = Dense (fun i j -> Complex.add (f1 i j) (f2 i j));
        }
    | Dense _, Sparse _ -> mat_plus m1 (mat_evaluate_dense m2)
    | Sparse _, Dense _ -> mat_plus (mat_evaluate_dense m1) m2
    | Sparse s1, Sparse s2 ->
        {
          r = m1.r;
          c = m1.c;
          ent =
            Sparse
              (Int2Map.merge
                 begin
                   fun _ z1opt z2opt ->
                     match (z1opt, z2opt) with
                     | Some z1, Some z2 -> Some (Complex.add z1 z2)
                     | Some z1, None -> Some z1
                     | None, Some z2 -> Some z2
                     | None, None -> None
                 end
                 s1 s2);
        }

let mat_minus (m1 : matrix) (m2 : matrix) : matrix =
  mat_plus m1 (mat_scalar_mul (Complex.neg Complex.one) m2)

let mat_transpose (m : matrix) : matrix =
  match m.ent with
  | Dense f -> { r = m.c; c = m.r; ent = Dense (fun i j -> f j i) }
  | Sparse s ->
      {
        r = m.c;
        c = m.r;
        ent =
          Sparse
            (Int2Map.of_seq
               (List.to_seq
                  (List.map
                     (fun ((i, j), z) -> ((j, i), z))
                     (Int2Map.bindings s))));
      }

let mat_conjugate (m : matrix) : matrix =
  match m.ent with
  | Dense f ->
      { r = m.r; c = m.c; ent = Dense (fun i j -> Complex.conj (f i j)) }
  | Sparse s -> { r = m.r; c = m.c; ent = Sparse (Int2Map.map Complex.conj s) }

let mat_adjoint (m : matrix) : matrix = mat_conjugate (mat_transpose m)

let rec mat_mul (m1 : matrix) (m2 : matrix) : matrix =
  if m1.c <> m2.r then
    invalid_arg
      (Printf.sprintf
         "Inconsistent matrix dimensions in matrix multiplication: %dx%d and \
          %dx%d"
         m1.r m1.c m2.r m2.c)
  else
    match (m1.ent, m2.ent) with
    | Dense f1, Dense f2 ->
        mat_evaluate_dense
          {
            r = m1.r;
            c = m2.c;
            ent =
              Dense
                (fun i j ->
                  complex_sum_to_n m1.c (fun k ->
                      Complex.mul (f1 i k) (f2 k j)));
          }
    | Dense _, Sparse _ -> mat_mul m1 (mat_evaluate_dense m2)
    | Sparse _, Dense _ -> mat_mul (mat_evaluate_dense m1) m2
    | Sparse s1, Sparse s2 ->
        {
          r = m1.r;
          c = m2.c;
          ent =
            begin
              let comb =
                List.flatten
                  (List.map
                     begin
                       fun ((i, k), z1) ->
                         List.filter_map
                           begin
                             fun ((k', j), z2) ->
                               if k = k' then
                                 Some ((i, j), Complex.mul z1 z2)
                               else
                                 None
                           end
                           (Int2Map.bindings s2)
                     end
                     (Int2Map.bindings s1))
              in
                Sparse
                  (List.fold_left
                     begin
                       fun cur ((i, j), z) ->
                         if Int2Map.mem (i, j) cur then
                           Int2Map.add (i, j)
                             (Complex.add (Int2Map.find (i, j) cur) z)
                             cur
                         else
                           Int2Map.add (i, j) z cur
                     end
                     Int2Map.empty comb)
            end;
        }

let ( *@ ) = mat_mul
let mat_outer (m : matrix) = m *@ mat_adjoint m

let rec mat_tensor (m1 : matrix) (m2 : matrix) : matrix =
  match (m1.ent, m2.ent) with
  | Dense f1, Dense f2 ->
      mat_evaluate_dense
        {
          r = m1.r * m2.r;
          c = m1.c * m2.c;
          ent =
            Dense
              (fun i j ->
                Complex.mul
                  (f1 (i / m2.r) (j / m2.c))
                  (f2 (i mod m2.r) (j mod m2.c)));
        }
  | Dense _, Sparse _ -> mat_tensor m1 (mat_evaluate_dense m2)
  | Sparse _, Dense _ -> mat_tensor (mat_evaluate_dense m1) m2
  | Sparse s1, Sparse s2 ->
      {
        r = m1.r * m2.r;
        c = m1.c * m2.c;
        ent =
          begin
            let comb =
              List.flatten
                (List.map
                   begin
                     fun ((i1, j1), z1) ->
                       List.map
                         begin
                           fun ((i2, j2), z2) ->
                             ( ((i1 * m2.r) + i2, (j1 * m2.c) + j2),
                               Complex.mul z1 z2 )
                         end
                         (Int2Map.bindings s2)
                   end
                   (Int2Map.bindings s1))
            in
              Sparse (Int2Map.of_seq (List.to_seq comb))
          end;
      }

let rec vec_dirsum (m1 : matrix) (m2 : matrix) : matrix =
  if m1.c <> 1 || m2.c <> 1 then
    failwith "Direct sum only applies to vectors"
  else
    match (m1.ent, m2.ent) with
    | Dense f1, Dense f2 ->
        {
          r = m1.r + m2.r;
          c = 1;
          ent = Dense (fun i _ -> if i < m1.r then f1 i 0 else f2 (i - m1.r) 0);
        }
    | Dense _, Sparse _ -> vec_dirsum m1 (mat_evaluate_dense m2)
    | Sparse _, Dense _ -> vec_dirsum (mat_evaluate_dense m1) m2
    | Sparse s1, Sparse s2 ->
        {
          r = m1.r + m2.r;
          c = 1;
          ent =
            Sparse
              (Int2Map.of_seq
                 (List.to_seq
                    (Int2Map.bindings s1
                    @ List.map
                        (fun ((i, j), z) -> ((i + m1.r, j), z))
                        (Int2Map.bindings s2))));
        }

let mat_column (m : matrix) (c : int) : matrix =
  match m.ent with
  | Dense f ->
      {
        r = m.r;
        c = 1;
        ent = Dense (fun i j -> if j = 0 then f i c else Complex.zero);
      }
  | Sparse s ->
      {
        r = m.r;
        c = 1;
        ent =
          Sparse
            (Int2Map.of_seq
               (List.to_seq
                  (List.filter_map
                     (fun ((i, j), z) ->
                       if j = c then Some ((i, 0), z) else None)
                     (Int2Map.bindings s))));
      }

let mat_trace (m : matrix) : Complex.t =
  if m.r <> m.c then
    failwith "Matrix must be square"
  else
    match m.ent with
    | Dense f -> complex_sum_to_n m.r (fun i -> f i i)
    | Sparse s ->
        complex_sum
          (List.map snd
             (Int2Map.bindings (Int2Map.filter (fun (i, j) _ -> i = j) s)))

let mat_to_scalar (m : matrix) : Complex.t =
  if m.r <> 1 || m.c <> 1 then
    failwith (Printf.sprintf "Matrix must be 1x1, instead got %dx%d" m.r m.c)
  else
    mat_trace m

let mat_from_basis_action (r : int) (c : int) (bfun : int -> matrix) =
  let cols = List.map bfun (range c) in
    if List.for_all mat_is_sparse cols then
      {
        r;
        c;
        ent =
          Sparse
            (Int2Map.of_seq
               (List.to_seq
                  begin
                    List.fold_left
                      begin
                        fun cur (j, col) ->
                          match col.ent with
                          | Sparse s ->
                              cur
                              @ List.map
                                  (fun ((i, _), z) -> ((i, j), z))
                                  (Int2Map.bindings s)
                          | _ -> failwith "Expected sparse"
                      end
                      []
                      (List.combine (range c) cols)
                  end));
      }
    else
      let cols = Array.of_list cols in
        mat_evaluate_dense
          {
            r;
            c;
            ent =
              Dense
                (fun i j ->
                  let bvec = cols.(j) in
                    if bvec.r <> r || bvec.c <> 1 then
                      failwith "Invalid basis action"
                    else
                      mat_entry bvec i 0);
          }

let mat_identity (n : int) =
  if n < 0 then
    invalid_arg "Matrix size must be nonnegative"
  else
    {
      r = n;
      c = n;
      ent =
        Sparse
          (Int2Map.of_seq
             (List.to_seq
                (List.map (fun i -> ((i, i), Complex.one)) (range n))));
    }

let mat_from_u3 (theta : float) (phi : float) (lambda : float) : matrix =
  {
    r = 2;
    c = 2;
    ent =
      Dense
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

let mat_zero (r : int) (c : int) = { r; c; ent = Sparse Int2Map.empty }
let vec_zero (dim : int) = mat_zero dim 1

let basis_column_vec (dim : int) (k : int) =
  if k < 0 || k >= dim then
    invalid_arg "Invalid basis vector"
  else
    { r = dim; c = 1; ent = Sparse (Int2Map.singleton (k, 0) Complex.one) }

let basis_row_vec (dim : int) (k : int) =
  if k < 0 || k >= dim then
    invalid_arg "Invalid basis vector"
  else
    { r = 1; c = dim; ent = Sparse (Int2Map.singleton (0, k) Complex.one) }

let mat_sum r c : matrix list -> matrix =
  List.fold_left mat_plus (mat_zero r c)

let mat_from_list (l : Complex.t list list) : matrix =
  let r = List.length l in
  let c = List.length (List.hd l) in
    mat_evaluate_dense
      { r; c; ent = Dense (fun i j -> List.nth (List.nth l i) j) }

let string_of_complex (z : Complex.t) =
  Printf.sprintf "%.3f%+.3fi" z.re z.im

let print_mat (m : matrix) : unit =
  if m.r = 0 || m.c = 0 then
    Printf.printf "Degenerate matrix: %dx%d\n" m.r m.c
  else if m.r = 1 && m.c = 1 then
    Printf.printf "%s\n" (string_of_complex (mat_entry m 0 0))
  else
    match m.ent with
    | Dense f -> begin
        let dim_limit = 8 in
          if m.r > dim_limit || m.c > dim_limit then
            Printf.printf "Too large to print: %dx%d matrix\n" m.r m.c
          else
            for i = 0 to m.r - 1 do
              for j = 0 to m.c - 1 do
                Printf.printf "%13s " (string_of_complex (f i j))
              done;
              Printf.printf "\n"
            done
      end
    | Sparse s -> begin
        Printf.printf "%s\n"
          begin
            string_of_list_custom " + " false
              begin
                fun ((i, j), z) ->
                  begin
                    let coeffstring =
                      if z = Complex.one then
                        ""
                      else if z = Complex.neg Complex.one then
                        "-"
                      else
                        Printf.sprintf "(%s)" (string_of_complex z)
                    in
                      if m.c = 1 then
                        Printf.sprintf "%s|%d>" coeffstring i
                      else if m.r = 1 then
                        Printf.sprintf "%s<%d|" coeffstring j
                      else
                        Printf.sprintf "%s|%d><%d|" coeffstring i j
                  end
              end
              (Int2Map.bindings s)
          end
      end

let mat_approx_equal (m1 : matrix) (m2 : matrix) : bool =
  m1.r = m2.r && m1.c = m2.c
  && List.for_all
       (fun i ->
         List.for_all
           (fun j ->
             float_approx_equal
               (Complex.norm
                  (Complex.sub (mat_entry m1 i j) (mat_entry m2 i j)))
               0.)
           (range m1.c))
       (range m1.r)

type superoperator_contents =
  (* to_row, to_col, from_row, from_col *)
  | Dense of (int -> int -> int -> int -> Complex.t)
  | Sparse of Complex.t Int4Map.t

type superoperator = {
  dim_to : int;
  dim_from : int;
  ent : superoperator_contents;
}

let superop_entry (super : superoperator) (i : int) (j : int) (k : int)
    (l : int) : Complex.t =
  match super.ent with
  | Dense f -> f i j k l
  | Sparse s -> begin
      match Int4Map.find_opt (i, j, k, l) s with
      | Some z -> z
      | None -> Complex.zero
    end

let superop_evaluate_dense (super : superoperator) : superoperator =
  let df_range = range_arr super.dim_from in
  let dt_range = range_arr super.dim_to in
  let arr =
    Array.map
      (fun i ->
        Array.map
          (fun j ->
            Array.map
              (fun k ->
                Array.map (fun l -> superop_entry super i j k l) df_range)
              df_range)
          dt_range)
      dt_range
  in
    {
      dim_to = super.dim_to;
      dim_from = super.dim_from;
      ent = Dense (fun i j k l -> arr.(i).(j).(k).(l));
    }

let rec superop_apply (super : superoperator) (m : matrix) : matrix =
  if m.r <> super.dim_from || m.c <> super.dim_from then
    failwith "Inconsistent dimensions in superoperator application"
  else
    match (super.ent, m.ent) with
    | Dense sf, Dense mf ->
        mat_evaluate_dense
          {
            r = super.dim_to;
            c = super.dim_to;
            ent =
              Dense
                (fun i j ->
                  complex_sum_to_n super.dim_from (fun k ->
                      complex_sum_to_n super.dim_from (fun l ->
                          Complex.mul (mf k l) (sf i j k l))));
          }
    | Dense _, Sparse _ -> superop_apply super (mat_evaluate_dense m)
    | Sparse _, Dense _ -> superop_apply (superop_evaluate_dense super) m
    | Sparse ss, Sparse ms ->
        {
          r = super.dim_to;
          c = super.dim_to;
          ent =
            begin
              let comb =
                List.flatten
                  (List.map
                     begin
                       fun ((k, l), z1) ->
                         List.filter_map
                           begin
                             fun ((i, j, k', l'), z2) ->
                               if k = k' && l = l' then
                                 Some ((i, j, k, l), Complex.mul z1 z2)
                               else
                                 None
                           end
                           (Int4Map.bindings ss)
                     end
                     (Int2Map.bindings ms))
              in
                Sparse
                  (List.fold_left
                     begin
                       fun cur ((i, j, _, _), z) ->
                         if Int2Map.mem (i, j) cur then
                           Int2Map.add (i, j)
                             (Complex.add (Int2Map.find (i, j) cur) z)
                             cur
                         else
                           Int2Map.add (i, j) z cur
                     end
                     Int2Map.empty comb)
            end;
        }

let superop_from_basis_action (dim_to : int) (dim_from : int)
    (bfun : int -> int -> matrix) : superoperator =
  let df_range = range dim_from in
  let bf_arr =
    List.map (fun k -> List.map (fun l -> bfun k l) df_range) df_range
  in
    if List.for_all (List.for_all mat_is_sparse) bf_arr then
      let bf_arr_indexed =
        List.flatten
          (List.map
             (fun k -> List.map (fun l -> (k, l, bfun k l)) df_range)
             df_range)
      in
        {
          dim_to;
          dim_from;
          ent =
            Sparse
              (Int4Map.of_seq
                 (List.to_seq
                    begin
                      List.fold_left
                        begin
                          fun cur (k, l, (m : matrix)) ->
                            match m.ent with
                            | Sparse s ->
                                cur
                                @ List.map
                                    (fun ((i, j), z) -> ((i, j, k, l), z))
                                    (Int2Map.bindings s)
                            | _ -> failwith "Expected sparse"
                        end
                        [] bf_arr_indexed
                    end));
        }
    else
      let bf_arr = Array.of_list (List.map Array.of_list bf_arr) in
        superop_evaluate_dense
          {
            dim_to;
            dim_from;
            ent =
              Dense
                (fun i j k l ->
                  let bmat = bf_arr.(k).(l) in
                    if bmat.r <> dim_to || bmat.c <> dim_to then
                      failwith "Invalid basis action"
                    else
                      mat_entry bmat i j);
          }

let superop_on_basis (super : superoperator) (k : int) (l : int) : matrix =
  match super.ent with
  | Dense f ->
      {
        r = super.dim_to;
        c = super.dim_to;
        ent = Dense (fun i j -> f i j k l);
      }
  | Sparse s ->
      {
        r = super.dim_to;
        c = super.dim_to;
        ent =
          Sparse
            (Int2Map.of_seq
               (List.to_seq
                  (List.filter_map
                     (fun ((i, j, k', l'), z) ->
                       if k' = k && l' = l then Some ((i, j), z) else None)
                     (Int4Map.bindings s))));
      }

let print_superop (super : superoperator) : unit =
  let dim_limit = 4 in
    if super.dim_from > dim_limit || super.dim_to > dim_limit then
      Printf.printf "Too large to print: %dx%d -> %dx%d superoperator\n"
        super.dim_from super.dim_from super.dim_to super.dim_to
    else
      for k = 0 to super.dim_from - 1 do
        for i = 0 to super.dim_to - 1 do
          for l = 0 to super.dim_from - 1 do
            for j = 0 to super.dim_to - 1 do
              Printf.printf "%13s "
                (string_of_complex (superop_entry super i j k l))
            done;
            Printf.printf "    "
          done;
          Printf.printf "\n"
        done;
        if k <> super.dim_from - 1 then
          Printf.printf "\n"
      done
