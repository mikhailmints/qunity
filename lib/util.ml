type 'a optionE = SomeE of 'a | NoneE of string

module StringSet = Set.Make (String)
module StringMap = Map.Make (String)
module IntSet = Set.Make (Int)
module IntMap = Map.Make (Int)

let option_of_optionE (optE : 'a optionE) : 'a option =
  match optE with
  | NoneE _ -> None
  | SomeE x -> Some x

let all_or_nothing (l : 'a option list) : 'a list option =
  if List.for_all (fun a -> a <> None) l then
    Some
      (List.map
         (fun a ->
           match a with
           | Some a' -> a'
           | None -> failwith "unreachable")
         l)
  else
    None

let fresh_string (prefix : string) (s : StringSet.t) : string =
  let rec fresh_string_helper (i : int) =
    let candidate = prefix ^ string_of_int i in
      if StringSet.mem candidate s then
        fresh_string_helper (i + 1)
      else
        candidate
  in
    fresh_string_helper 0

let fresh_int_list (s : IntSet.t) (n : int) : int list * IntSet.t =
  let rec fresh_int_list_helper (i : int) (curlist : int list)
      (curset : IntSet.t) (n : int) =
    if n <= 0 then
      (curlist, curset)
    else if IntSet.mem i curset then
      fresh_int_list_helper (i + 1) curlist curset n
    else
      fresh_int_list_helper (i + 1) (i :: curlist) (IntSet.add i curset) (n - 1)
  in
  let res, s' = fresh_int_list_helper 0 [] s n in
    (List.rev res, s')

let rec fresh_int_lists (s : IntSet.t) (l : int list) :
    int list list * IntSet.t =
  match l with
  | [] -> ([], s)
  | n :: t ->
      let cur, s' = fresh_int_list s n in
      let rest, s'' = fresh_int_lists s' t in
        (cur :: rest, s'')

let fresh_int (s : IntSet.t) : int * IntSet.t =
  let i, res = fresh_int_list s 1 in
    (List.hd i, res)

(*
Combine two maps into one. If allow_dup is false: fails if
the maps share any bindings. If allow_dup is false: fails only if
there is a type mismatch.
*)
let map_merge (allow_dup : bool) (d0 : 'a StringMap.t) (d1 : 'a StringMap.t) :
    'a StringMap.t optionE =
  let merged_with_opt =
    StringMap.merge
      (fun _ a b ->
        match (a, b) with
        | None, None -> None
        | Some a', None -> Some (Some a')
        | None, Some b' -> Some (Some b')
        | Some a', Some b' ->
            if allow_dup && a' = b' then Some (Some a') else Some None)
      d0 d1
  in
    if StringMap.for_all (fun _ x -> x <> None) merged_with_opt then
      SomeE
        (StringMap.map
           (fun x ->
             match x with
             | Some x' -> x'
             | None -> failwith "unreachable")
           merged_with_opt)
    else
      NoneE
        (if allow_dup then
           "Type mismatch in context merge"
         else
           "Expected disjoint contexts")

(* Whether a map is included in another map *)
let map_inclusion (d0 : 'a StringMap.t) (d1 : 'a StringMap.t) : bool =
  StringMap.for_all (fun x t -> StringMap.find_opt x d1 = Some t) d0

let map_dom (d : 'a StringMap.t) : StringSet.t =
  StringSet.of_list (List.map fst (StringMap.bindings d))

(* Restrict map to include only variables from a given set *)
let map_restriction (d : 'a StringMap.t) (s : StringSet.t) : 'a StringMap.t =
  StringMap.filter (fun x _ -> StringSet.mem x s) d

(* Partition map d into part belonging to s and part not belonging to s *)
let map_partition (d : 'a StringMap.t) (s : StringSet.t) :
    'a StringMap.t * 'a StringMap.t =
  (map_restriction d s, StringMap.filter (fun x _ -> not (StringSet.mem x s)) d)

let int_map_find_or_keep (i : int) (m : int IntMap.t) : int =
  match IntMap.find_opt i m with
  | Some j -> j
  | None -> i

let int_list_union (l1 : int list) (l2 : int list) : int list =
  IntSet.elements (IntSet.union (IntSet.of_list l1) (IntSet.of_list l2))

let int_list_intersection (l1 : int list) (l2 : int list) : int list =
  IntSet.elements (IntSet.inter (IntSet.of_list l1) (IntSet.of_list l2))

let int_list_diff (l1 : int list) (l2 : int list) : int list =
  IntSet.elements (IntSet.diff (IntSet.of_list l1) (IntSet.of_list l2))

let rec int_list_max (l : int list) : int =
  match l with
  | [] -> Int.min_int
  | h :: t -> max h (int_list_max t)

let string_of_list (f : 'a -> string) (l : 'a list) =
  let rec string_of_int_list_helper l =
    match l with
    | [] -> ""
    | [h] -> f h
    | h :: t -> f h ^ ", " ^ string_of_int_list_helper t
  in
    "[" ^ string_of_int_list_helper l ^ "]"

let list_index (cmp : 'a -> 'a -> bool) (l : 'a list) (x : 'a) : int =
  let rec iter (l : 'a list) (i : int) : int =
    match l with
    | [] -> failwith "Not found"
    | x' :: l' -> if cmp x' x then i else iter l' (i + 1)
  in
    iter l 0

let rec list_constant (x : 'a) (n : int) : 'a list =
  if n <= 0 then
    []
  else
    x :: list_constant x (n - 1)

let list_split_at_i (l : 'a list) (i : int) : 'a list * 'a list =
  let rec iter (cur : 'a list) (rest : 'a list) (i : int) =
    if i <= 0 then
      (cur, rest)
    else if rest = [] then
      failwith "list_split_at_i: index out of range"
    else
      iter (List.hd rest :: cur) (List.tl rest) (i - 1)
  in
  let l0, l1 = iter [] l i in
    (List.rev l0, l1)

let range (i : int) : int list =
  let rec iter i = if i <= 0 then [] else (i - 1) :: iter (i - 1) in
    List.rev (iter i)

let float_approx_equal (a : float) (b : float) : bool =
  let eps = 1e-15 in
    Float.abs (a -. b) <= eps
