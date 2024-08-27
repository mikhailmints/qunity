(** An option type that can also contain an error message. *)
type 'a optionE = SomeE of 'a | NoneE of string

module StringSet = Set.Make (String)
module StringMap = Map.Make (String)
module IntSet = Set.Make (Int)
module IntMap = Map.Make (Int)

let fst3 (a, _, _) = a
let snd3 (_, b, _) = b
let trd3 (_, _, c) = c

(** Converts [SomeE] to [Some] and [NoneE] to [None]. *)
let option_of_optionE (optE : 'a optionE) : 'a option =
  match optE with
  | NoneE _ -> None
  | SomeE x -> Some x

(** Outputs [None] if any of the elements of [l] are [None], otherwise remove
    the option constructors from the elements. *)
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

(** Outputs [None] if any of the values of [d] are [None], otherwise remove the
    option constructors from the values. *)
let map_all_or_nothing (d : 'a option StringMap.t) : 'a StringMap.t option =
  let keys, vals = List.split (StringMap.bindings d) in
    match all_or_nothing vals with
    | Some l -> Some (StringMap.of_seq (List.to_seq (List.combine keys l)))
    | _ -> None

(** Creates a string that does not appear in the set [s], starting with
    [prefix]. *)
let fresh_string (prefix : string) (s : StringSet.t) : string =
  let rec fresh_string_helper (i : int) =
    let candidate = prefix ^ string_of_int i in
      if StringSet.mem candidate s then
        fresh_string_helper (i + 1)
      else
        candidate
  in
    fresh_string_helper 0

(** Creates a list of integers of a given length [n] containing elements not
    appearing in the set [s]. Returns the list and the updated set with the new
    elements. *)
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

(** Creates a list of lists of integers of given lengths in [l], containing
    elements not appearing in the set [s]. Returns the list and the updated set
    with the new elements. *)
let rec fresh_int_lists (s : IntSet.t) (l : int list) :
    int list list * IntSet.t =
  match l with
  | [] -> ([], s)
  | n :: t ->
      let cur, s' = fresh_int_list s n in
      let rest, s'' = fresh_int_lists s' t in
        (cur :: rest, s'')

(** Finds a single integer not appearing in the set [s]. Returns the integer
    and the updated set with the new element. *)
let fresh_int (s : IntSet.t) : int * IntSet.t =
  let i, res = fresh_int_list s 1 in
    (List.hd i, res)

(** Combines two maps into one. If [allow_dup] is [false]: fails if the maps
    share any bindings. If [allow_dup] is [true]: fails only if there is a
    value mismatch. *)
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
           "Value mismatch in map merge"
         else
           "Expected disjoint maps")

(** Merges two maps, throwing an exception if it is impossible to merge them. *)
let map_merge_noopt (allow_dup : bool) (d0 : 'a StringMap.t)
    (d1 : 'a StringMap.t) : 'a StringMap.t =
  match map_merge allow_dup d0 d1 with
  | SomeE res -> res
  | NoneE err -> failwith err

(** Checks whether a map is included in another map. *)
let map_is_inclusion (d0 : 'a StringMap.t) (d1 : 'a StringMap.t) : bool =
  StringMap.for_all (fun x t -> StringMap.find_opt x d1 = Some t) d0

(** Returns the keys of the given map. *)
let map_dom (d : 'a StringMap.t) : StringSet.t =
  StringSet.of_list (List.map fst (StringMap.bindings d))

(** Restricts map to include only variables from a given set. *)
let map_restriction (d : 'a StringMap.t) (s : StringSet.t) : 'a StringMap.t =
  StringMap.filter (fun x _ -> StringSet.mem x s) d

(** Restricts map to include only variables not in a given set. *)
let map_exclusion (d : 'a StringMap.t) (s : StringSet.t) : 'a StringMap.t =
  StringMap.filter (fun x _ -> not (StringSet.mem x s)) d

(** Partitions map [d] into a part belonging to [s] and a part not belonging to
    [s]. *)
let map_partition (d : 'a StringMap.t) (s : StringSet.t) :
    'a StringMap.t * 'a StringMap.t =
  (map_restriction d s, map_exclusion d s)

(** If [i] occurs as a key in [m], return the corresponding value. Otherwise,
    return [i]. *)
let int_map_find_or_keep (m : int IntMap.t) (i : int) : int =
  match IntMap.find_opt i m with
  | Some j -> j
  | None -> i

(** Intersection between two integer lists, keeping the order in [l1]. *)
let int_list_intersection (l1 : int list) (l2 : int list) : int list =
  let l2_set = IntSet.of_list l2 in
    List.filter (fun x -> IntSet.mem x l2_set) l1

(** Difference between two integer lists, keeping the order in [l1]. *)
let int_list_diff (l1 : int list) (l2 : int list) : int list =
  let l2_set = IntSet.of_list l2 in
    List.filter (fun x -> not (IntSet.mem x l2_set)) l1

(** Union of two integer lists, keeping the order in [l1] and adding the
    elements of [l2] not in [l1], in the order of [l2]. *)
let int_list_union (l1 : int list) (l2 : int list) : int list =
  l1 @ int_list_diff l2 l1

(** Finds the maximum element in an integer list. *)
let rec int_list_max (l : int list) : int =
  match l with
  | [] -> Int.min_int
  | h :: t -> max h (int_list_max t)

(** Converts a list to a string, taking in a custom delimiter, an option to
    include or exclude brackets, and a function for converting the elements to
    strings. *)
let string_of_list_custom (delimiter : string) (brackets : bool)
    (f : 'a -> string) (l : 'a list) =
  let rec string_of_list_helper l =
    match l with
    | [] -> ""
    | [h] -> f h
    | h :: t -> f h ^ delimiter ^ string_of_list_helper t
  in
  let s = string_of_list_helper l in
    if brackets then
      "[" ^ s ^ "]"
    else
      s

(** Converts a list to a string, using semicolon delimiters and brackets,
    taking in a function for converting the elements to strings. *)
let string_of_list (f : 'a -> string) (l : 'a list) =
  string_of_list_custom "; " true f l

(** Finds the index of an element in a list. *)
let list_index (cmp : 'a -> 'a -> bool) (l : 'a list) (x : 'a) : int =
  let rec iter (l : 'a list) (i : int) : int =
    match l with
    | [] -> failwith "Not found"
    | x' :: l' -> if cmp x' x then i else iter l' (i + 1)
  in
    iter l 0

(** Creates a list of a constant value [x] with a given length [n]. *)
let rec list_constant (x : 'a) (n : int) : 'a list =
  if n <= 0 then
    []
  else
    x :: list_constant x (n - 1)

(** Splits a list [l] into two parts, with the first part having length [i]. *)
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

(** Splits a list of triples into three lists. *)
let rec list_split3 (l : ('a * 'b * 'c) list) : 'a list * 'b list * 'c list =
  match l with
  | [] -> ([], [], [])
  | (a, b, c) :: l' ->
      let la, lb, lc = list_split3 l' in
        (a :: la, b :: lb, c :: lc)

(** Returns the numbers from [0] (inclusive) to [i] (exclusive), as a list. *)
let range (i : int) : int list =
  let rec iter i = if i <= 0 then [] else (i - 1) :: iter (i - 1) in
    List.rev (iter i)

(** Returns the numbers from [0] (inclusive) to [i] (exclusive), as an array. *)
let range_arr (i : int) : int array = Array.of_list (range i)

(** Tests whether two floats are approximately equal. *)
let float_approx_equal (a : float) (b : float) : bool =
  let eps = 1e-15 in
    Float.abs (a -. b) <= eps
