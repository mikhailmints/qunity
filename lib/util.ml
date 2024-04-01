type 'a optionE = SomeE of 'a | NoneE of string

module StringSet = Set.Make (String)
module StringMap = Map.Make (String)

let option_of_optionE (optE : 'a optionE) : 'a option =
  match optE with
  | NoneE _ -> None
  | SomeE x -> Some x

let fresh_string (s : StringSet.t) : string =
  let rec fresh_string_helper (i : int) =
    let candidate = "$" ^ string_of_int i in
      if StringSet.mem candidate s then
        fresh_string_helper (i + 1)
      else
        candidate
  in
    fresh_string_helper 0

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

let list_index (l : 'a list) (x : 'a) : int =
  let rec iter (l : 'a list) (i : int) : int =
    match l with
    | [] -> failwith "Not found"
    | x' :: l' -> if x' = x then i else iter l' (i + 1)
  in
    iter l 0

let range (i : int) : int list =
  let rec iter i =
    if i <= 0 then [] else (i - 1) :: iter (i - 1)
  in
    List.rev (iter i)
