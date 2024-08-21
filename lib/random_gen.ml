open Util
open Reals
open Syntax

exception Random_gen_failure

let _ = Random.self_init ()

type random_gen_settings = {
  type_deepen_prob : float;
  type_decay_rate : float;
  deepen_prob : float;
  decay_rate : float;
  retry_if_failure_prob : float;
  deepen_if_stopping_will_error_prob : float;
  bit_type_prob : float;
  prod_type_prob : float;
  void_type_prob : float;
  allow_quantum : bool;
  allow_mixed : bool;
  cvar_weight : float;
  qvar_weight : float;
  pair_weight : float;
  ctrl_weight : float;
  trycatch_weight : float;
  apply_weight : float;
  u3_weight : float;
  left_right_weight : float;
  lambda_weight : float;
  rphase_weight : float;
  apply_keep_same_type_weight : float;
  apply_take_type_part_weight : float;
  apply_take_random_type_weight : float;
  pattern_make_var_prob : float;
}

let random_gen_default_settings =
  {
    type_deepen_prob = 0.5;
    type_decay_rate = 1.;
    deepen_prob = 1.;
    decay_rate = 0.75;
    retry_if_failure_prob = 0.75;
    deepen_if_stopping_will_error_prob = 0.5;
    bit_type_prob = 0.75;
    prod_type_prob = 0.75;
    void_type_prob = 0.05;
    allow_quantum = true;
    allow_mixed = true;
    cvar_weight = 1.;
    qvar_weight = 1.;
    pair_weight = 0.5;
    ctrl_weight = 0.;
    trycatch_weight = 0.25;
    apply_weight = 1.;
    u3_weight = 1.;
    left_right_weight = 0.5;
    lambda_weight = 1.;
    rphase_weight = 0.2;
    apply_keep_same_type_weight = 1.;
    apply_take_type_part_weight = 0.;
    apply_take_random_type_weight = 0.;
    pattern_make_var_prob = 0.75;
  }

let list_random_split (l : 'a list) : 'a list * 'a list =
  let i =
    if List.length l = 0 then
      0
    else
      Random.int (List.length l)
  in
    list_split_at_i l i

let map_random_split (d : 'a StringMap.t) : 'a StringMap.t * 'a StringMap.t =
  let d_bindings_tagged =
    List.map (fun x -> (Random.bits (), x)) (StringMap.bindings d)
  in
  let d_bindings_shuffled =
    List.map snd (List.sort (fun (a, _) (b, _) -> a - b) d_bindings_tagged)
  in
  let l0, l1 = list_random_split d_bindings_shuffled in
    (StringMap.of_list l0, StringMap.of_list l1)

let random_angle () : real =
  let denom = 64 in
  let num = Random.int denom in
    Times (Div (Const num, Const denom), Times (Const 2, Pi))

let rec random_type (settings : random_gen_settings) : exprtype =
  if Random.float 1. > settings.type_deepen_prob then
    if Random.float 1. < settings.bit_type_prob then
      bit
    else if Random.float 1. < settings.void_type_prob then
      Void
    else
      Qunit
  else
    let next_settings =
      {
        settings with
        type_deepen_prob =
          settings.type_deepen_prob *. settings.type_decay_rate;
      }
    in
      if Random.float 1. < settings.prod_type_prob then
        ProdType (random_type next_settings, random_type next_settings)
      else
        SumType (random_type next_settings, random_type next_settings)

let rec random_basis_expr_of_type (t : exprtype) : expr =
  match t with
  | Void -> Ctrl (Null, Qunit, [], Void)
  | Qunit -> Null
  | SumType (t0, t1) ->
      if Random.float 1. < 0.5 then
        Apply (Left (t0, t1), random_basis_expr_of_type t0)
      else
        Apply (Right (t0, t1), random_basis_expr_of_type t1)
  | ProdType (t0, t1) ->
      Qpair (random_basis_expr_of_type t0, random_basis_expr_of_type t1)

let rec random_pattern_expr_of_type (settings : random_gen_settings)
    (t : exprtype) (d : context) : expr * context =
  if Random.float 1. < settings.pattern_make_var_prob then
    let name = fresh_string "_" (map_dom d) in
      (Var name, StringMap.add name t d)
  else
    match t with
    | Void -> (Ctrl (Null, Qunit, [], Void), d)
    | Qunit -> (Null, d)
    | SumType (t0, t1) ->
        if Random.float 1. < 0.5 then
          let e0, d = random_pattern_expr_of_type settings t0 d in
            (Apply (Left (t0, t1), e0), d)
        else
          let e1, d = random_pattern_expr_of_type settings t1 d in
            (Apply (Right (t0, t1), e1), d)
    | ProdType (t0, t1) ->
        let e0, d = random_pattern_expr_of_type settings t0 d in
        let e1, d = random_pattern_expr_of_type settings t1 d in
          (Qpair (e0, e1), d)

let rec _random_expr_of_type (settings : random_gen_settings) (t : exprtype)
    (g : context) (d : context) : expr =
  let next_settings =
    { settings with deepen_prob = settings.deepen_prob *. settings.decay_rate }
  in
  let g_of_t =
    StringSet.to_list (map_dom (StringMap.filter (fun _ t' -> t' = t) g))
  in
  let d_of_t =
    StringSet.to_list (map_dom (StringMap.filter (fun _ t' -> t' = t) d))
  in
    if Random.float 1. > settings.deepen_prob || t = Void || t = Qunit then
      if StringMap.cardinal d = 0 then
        random_basis_expr_of_type t
      else if List.length d_of_t = 1 && StringMap.cardinal d = 1 then
        Var (List.hd d_of_t)
      else if Random.float 1. < settings.deepen_if_stopping_will_error_prob
      then
        random_expr_of_type next_settings t g d
      else if not settings.allow_mixed then
        raise Random_gen_failure
      else
        let d_names, d_types = List.split (StringMap.bindings d) in
        let combined_type =
          List.fold_right (fun a b -> ProdType (a, b)) d_types Qunit
        in
        let new_names =
          fresh_string_list "_" StringSet.empty (List.length d_types)
        in
        let new_names_expr =
          List.fold_right (fun a b -> Qpair (Var a, b)) new_names Null
        in
        let d_names_expr =
          List.fold_right (fun a b -> Qpair (Var a, b)) d_names Null
        in
          Apply
            ( Lambda
                (new_names_expr, combined_type, random_basis_expr_of_type t),
              d_names_expr )
    else
      let cvar_weight =
        if List.length g_of_t <> 0 then settings.cvar_weight else 0.
      in
      let qvar_weight =
        if List.length d_of_t = 1 && StringMap.cardinal d = 1 then
          settings.qvar_weight
        else
          0.
      in
      let pair_weight =
        if
          match t with
          | ProdType _ -> true
          | _ -> false
        then
          settings.pair_weight
        else
          0.
      in
      let ctrl_weight = settings.ctrl_weight in
      let trycatch_weight =
        if settings.allow_mixed then settings.trycatch_weight else 0.
      in
      let apply_weight = settings.apply_weight in
      let total_weight =
        cvar_weight +. qvar_weight +. pair_weight +. ctrl_weight
        +. trycatch_weight +. apply_weight
      in
        if total_weight = 0. then
          raise Random_gen_failure;
        let random = Random.float total_weight in
          if random < cvar_weight then
            Var (List.nth g_of_t (Random.int (List.length g_of_t)))
          else if random < cvar_weight +. qvar_weight then
            Var (List.hd d_of_t)
          else if random < cvar_weight +. qvar_weight +. pair_weight then
            match t with
            | ProdType (t0, t1) -> begin
                let d_shared, d_unshared = map_random_split d in
                let d_unshared0, d_unshared1 = map_random_split d_unshared in
                let d0 = map_merge_noopt false d_shared d_unshared0 in
                let d1 = map_merge_noopt false d_shared d_unshared1 in
                  Qpair
                    ( random_expr_of_type next_settings t0 g d0,
                      random_expr_of_type next_settings t1 g d1 )
              end
            | _ -> failwith "Expected product type"
          else if
            random < cvar_weight +. qvar_weight +. pair_weight +. ctrl_weight
          then
            failwith "ctrl"
          else if
            random
            < cvar_weight +. qvar_weight +. pair_weight +. ctrl_weight
              +. trycatch_weight
          then
            let d0, d1 = map_random_split (map_merge_noopt false g d) in
              Try
                ( random_expr_of_type next_settings t StringMap.empty d0,
                  random_expr_of_type next_settings t StringMap.empty d1 )
          else
            let total_apply_type_weight =
              settings.apply_keep_same_type_weight
              +. settings.apply_take_type_part_weight
              +. settings.apply_take_random_type_weight
            in
            let apply_type_random = Random.float total_apply_type_weight in
            let apply_type =
              begin
                if apply_type_random < settings.apply_keep_same_type_weight
                then
                  t
                else if
                  apply_type_random
                  < settings.apply_keep_same_type_weight
                    +. settings.apply_take_type_part_weight
                then
                  match t with
                  | Void -> Void
                  | Qunit -> Qunit
                  | SumType (t0, t1) ->
                      if Random.float 1. < 0.5 then t0 else t1
                  | ProdType (t0, t1) ->
                      if Random.float 1. < 0.5 then t0 else t1
                else
                  random_type next_settings
              end
            in
              Apply
                ( random_prog next_settings apply_type t,
                  random_expr_of_type next_settings apply_type g d )

and random_expr_of_type (settings : random_gen_settings) (t : exprtype)
    (g : context) (d : context) : expr =
  try _random_expr_of_type settings t g d with
  | Random_gen_failure ->
      if Random.float 1. < settings.retry_if_failure_prob then
        random_expr_of_type settings t g d
      else
        raise Random_gen_failure

and random_prog (settings : random_gen_settings) (t0 : exprtype)
    (t1 : exprtype) =
  let u3_weight =
    if t0 = bit && t1 = bit && settings.allow_quantum then
      settings.u3_weight
    else
      0.
  in
  let left_weight =
    if
      match t1 with
      | SumType (t0', _) when t0' = t0 -> true
      | _ -> false
    then
      settings.left_right_weight
    else
      0.
  in
  let right_weight =
    if
      match t1 with
      | SumType (_, t0') when t0' = t0 -> true
      | _ -> false
    then
      settings.left_right_weight
    else
      0.
  in
  let lambda_weight = settings.lambda_weight in
  let rphase_weight =
    if settings.allow_quantum && t0 = t1 then settings.rphase_weight else 0.
  in
  let total_weight =
    u3_weight +. left_weight +. right_weight +. lambda_weight +. rphase_weight
  in
    if total_weight = 0. then
      raise Random_gen_failure;
    let random = Random.float total_weight in
      if random < u3_weight then
        U3 (random_angle (), random_angle (), random_angle ())
      else if random < u3_weight +. left_weight then
        match t1 with
        | SumType (t0', t') when t0' = t0 -> Left (t0', t')
        | _ -> failwith "unreachable"
      else if random < u3_weight +. left_weight +. right_weight then
        match t1 with
        | SumType (t', t0') when t0' = t0 -> Right (t', t0')
        | _ -> failwith "unreachable"
      else if
        random < u3_weight +. left_weight +. right_weight +. lambda_weight
      then
        let pattern, d =
          random_pattern_expr_of_type settings t0 StringMap.empty
        in
        let d_body =
          if settings.allow_mixed then
            fst (map_random_split d)
          else
            d
        in
        let body = random_expr_of_type settings t1 StringMap.empty d_body in
          Lambda (pattern, t0, body)
      else
        let e =
          random_expr_of_type
            { settings with allow_mixed = false }
            t0 StringMap.empty StringMap.empty
        in
          Rphase (t0, e, random_angle (), random_angle ())
