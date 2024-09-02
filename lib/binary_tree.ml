open Util

(** A simple binary tree data structure, used for representing direct sum
    encoding structures. *)
type binary_tree = Leaf | Node of binary_tree * binary_tree

(** A binary tree storing data in the leaves. *)
type 'a valued_binary_tree =
  | ValuedLeaf of 'a
  | ValuedNode of 'a valued_binary_tree * 'a valued_binary_tree

(** A transformation applied to a binary tree, in a form which can easily be
    converted into quantum operators acting on sum types. *)
type tree_transformation =
  | TreeIdentity
  | TreeLeftRotation
  | TreeRightRotation
  | TreeCommute
  | TreeLeftApply of tree_transformation
  | TreeRightApply of tree_transformation
  | TreeBothApply of tree_transformation * tree_transformation
  | TreeConditionalCommute of bool list
  | TreeSequence of tree_transformation * tree_transformation

(** String representation of a binary tree. *)
let rec string_of_tree (tree : binary_tree) : string =
  match tree with
  | Leaf -> "Leaf"
  | Node (tree0, tree1) ->
      Printf.sprintf "Node (%s, %s)" (string_of_tree tree0)
        (string_of_tree tree1)

(** String representation of a valued binary tree. *)
let rec string_of_valued_tree (f : 'a -> string) (tree : 'a valued_binary_tree)
    : string =
  match tree with
  | ValuedLeaf x -> "ValuedLeaf " ^ f x
  | ValuedNode (tree0, tree1) ->
      Printf.sprintf "ValuedNode (%s, %s)"
        (string_of_valued_tree f tree0)
        (string_of_valued_tree f tree1)

(** The number of leaves in the tree. *)
let rec tree_size (tree : binary_tree) : int =
  match tree with
  | Leaf -> 1
  | Node (l, r) -> tree_size l + tree_size r

(** The longest distance from the root of the tree to a leaf. *)
let rec tree_height (tree : binary_tree) : int =
  match tree with
  | Leaf -> 0
  | Node (l, r) -> 1 + max (tree_height l) (tree_height r)

(** Removes the values from a valued binary tree. *)
let rec tree_valued_to_normal (tree : 'a valued_binary_tree) : binary_tree =
  match tree with
  | ValuedLeaf _ -> Leaf
  | ValuedNode (l, r) -> Node (tree_valued_to_normal l, tree_valued_to_normal r)

(** Converts a binary tree to a valued one by inserting unit values. *)
let rec tree_normal_to_valued (tree : binary_tree) : unit valued_binary_tree =
  match tree with
  | Leaf -> ValuedLeaf ()
  | Node (l, r) -> ValuedNode (tree_normal_to_valued l, tree_normal_to_valued r)

(** Converts a binary tree to a valued one by inserting values specified in the
    input list. *)
let rec assign_values_to_tree (tree : binary_tree) (l : 'a list) :
    'a valued_binary_tree =
  match (tree, l) with
  | _, [] -> failwith "Expected nonempty list"
  | Leaf, [x] -> ValuedLeaf x
  | Leaf, _ -> failwith "Expected non-leaf"
  | Node _, [_] -> failwith "Expected leaf"
  | Node (tree0, tree1), _ -> begin
      let n_left = tree_size tree0 in
      let l_left, l_right = list_split_at_i l n_left in
        ValuedNode
          ( assign_values_to_tree tree0 l_left,
            assign_values_to_tree tree1 l_right )
    end

(** Obtains the list of values from a valued tree. *)
let rec tree_values (tree : 'a valued_binary_tree) : 'a list =
  match tree with
  | ValuedLeaf x -> [x]
  | ValuedNode (l, r) -> tree_values l @ tree_values r

(** To each leaf of [tree0], attach a copy of the corresponding element of
    [l_tree1]. It is required that the length of [l_tree1] is the same as the
    size of [tree0]. *)
let rec tree_multiply (tree0 : binary_tree) (l_tree1 : binary_tree list) :
    binary_tree =
  match (tree0, l_tree1) with
  | Leaf, [tree1] -> tree1
  | Leaf, _ -> failwith "Mismatch when multiplying trees"
  | Node (tree0_l, tree0_r), _ -> begin
      let l_tree1_l, l_tree1_r = list_split_at_i l_tree1 (tree_size tree0_l) in
        Node (tree_multiply tree0_l l_tree1_l, tree_multiply tree0_r l_tree1_r)
    end

(** Given a value, assumed to occur exactly once in the given tree, finds the
    path from the root to the value and returns it as a [bool list], where an
    entry of [false] corresponds to going to the left child, and an entry of
    [true] corresponds to going to the right child. *)
let rec tree_get_path (tree : 'a valued_binary_tree) (x : 'a) :
    bool list option =
  match tree with
  | ValuedLeaf x' -> if x' = x then Some [] else None
  | ValuedNode (l, r) -> begin
      match (tree_get_path l x, tree_get_path r x) with
      | Some path, None -> Some (false :: path)
      | None, Some path -> Some (true :: path)
      | Some _, Some _ -> failwith "Duplicate values found in tree"
      | _ -> None
    end

(** Given a tree and a path, returns the subtree obtained by walking along the
    path. *)
let rec valued_tree_get_subtree_from_path (tree : 'a valued_binary_tree)
    (path : bool list) : 'a valued_binary_tree =
  match (tree, path) with
  | _, [] -> tree
  | ValuedLeaf _, _ -> failwith "Expected empty path"
  | ValuedNode (l, r), b :: p ->
      if b then
        valued_tree_get_subtree_from_path r p
      else
        valued_tree_get_subtree_from_path l p

(** Replaces the subtree located at a given path with a new [replacement] tree. *)
let rec valued_tree_replace_at_path (tree : 'a valued_binary_tree)
    (path : bool list) (replacement : 'a valued_binary_tree) :
    'a valued_binary_tree =
  match (tree, path) with
  | _, [] -> replacement
  | ValuedLeaf _, _ -> failwith "Expected empty path"
  | ValuedNode (l, r), b :: p ->
      if b then
        ValuedNode (l, valued_tree_replace_at_path r p replacement)
      else
        ValuedNode (valued_tree_replace_at_path l p replacement, r)

(** Creates a tree transformation that the given transformation to the subtree
    located at the given path. *)
let rec tree_path_apply (path : bool list) (trans : tree_transformation) :
    tree_transformation =
  match path with
  | [] -> trans
  | false :: p -> TreeLeftApply (tree_path_apply p trans)
  | true :: p -> TreeRightApply (tree_path_apply p trans)

(** Given a tree transformation, finds its inverse. Left and right rotations
    are inverses of each other, sequences get reversed, and the other
    transformations are involutive. *)
let rec tree_transformation_inverse (trans : tree_transformation) :
    tree_transformation =
  match trans with
  | TreeIdentity -> TreeIdentity
  | TreeLeftRotation -> TreeRightRotation
  | TreeRightRotation -> TreeLeftRotation
  | TreeCommute -> TreeCommute
  | TreeLeftApply trans' -> TreeLeftApply (tree_transformation_inverse trans')
  | TreeRightApply trans' ->
      TreeRightApply (tree_transformation_inverse trans')
  | TreeBothApply (trans0, trans1) ->
      TreeBothApply
        (tree_transformation_inverse trans0, tree_transformation_inverse trans1)
  | TreeConditionalCommute path -> TreeConditionalCommute path
  | TreeSequence (trans0, trans1) ->
      TreeSequence
        (tree_transformation_inverse trans1, tree_transformation_inverse trans0)

(** Applies a tree transformation to a valued binary tree, returning a new
    tree. *)
let rec transform_valued_tree (trans : tree_transformation)
    (tree : 'a valued_binary_tree) : 'a valued_binary_tree =
  match tree with
  | ValuedLeaf _ -> tree
  | ValuedNode (l, r) -> begin
      match trans with
      | TreeIdentity -> tree
      | TreeLeftRotation -> begin
          match r with
          | ValuedLeaf _ ->
              failwith "Cannot apply left rotation when right child is leaf"
          | ValuedNode (rl, rr) -> ValuedNode (ValuedNode (l, rl), rr)
        end
      | TreeRightRotation -> begin
          match l with
          | ValuedLeaf _ ->
              failwith "Cannot apply right rotation when left child is leaf"
          | ValuedNode (ll, lr) -> ValuedNode (ll, ValuedNode (lr, r))
        end
      | TreeCommute -> ValuedNode (r, l)
      | TreeLeftApply trans' -> ValuedNode (transform_valued_tree trans' l, r)
      | TreeRightApply trans' -> ValuedNode (l, transform_valued_tree trans' r)
      | TreeBothApply (l_trans, r_trans) ->
          ValuedNode
            (transform_valued_tree l_trans l, transform_valued_tree r_trans r)
      | TreeConditionalCommute path -> begin
          let path0 = false :: path in
          let path1 = true :: path in
          let tree0 = valued_tree_get_subtree_from_path tree path0 in
          let tree1 = valued_tree_get_subtree_from_path tree path1 in
            valued_tree_replace_at_path
              (valued_tree_replace_at_path tree path0 tree1)
              path1 tree0
        end
      | TreeSequence (trans0, trans1) ->
          transform_valued_tree trans1 (transform_valued_tree trans0 tree)
    end

(** Applies a tree transformation to a binary tree, returning a new tree. *)
let transform_tree (trans : tree_transformation) (tree : binary_tree) :
    binary_tree =
  tree |> tree_normal_to_valued
  |> transform_valued_tree trans
  |> tree_valued_to_normal

(** Creates a transformation that transforms a tree in such a way that it will
    have a leaf as its left child. *)
let rec bring_leaf_to_top_left (tree : binary_tree) : tree_transformation =
  match tree with
  | Leaf -> failwith "Cannot bring leaf to top left of leaf"
  | Node (l, _) -> begin
      match l with
      | Leaf -> TreeIdentity
      | _ ->
          TreeSequence
            (TreeLeftApply (bring_leaf_to_top_left l), TreeRightRotation)
    end

(** Creates a transformation that transforms a tree in such a way that it will
    have a leaf as its right child. *)
let rec bring_leaf_to_top_right (tree : binary_tree) : tree_transformation =
  match tree with
  | Leaf -> failwith "Cannot bring leaf to top right of leaf"
  | Node (_, r) -> begin
      match r with
      | Leaf -> TreeIdentity
      | _ ->
          TreeSequence
            (TreeRightApply (bring_leaf_to_top_right r), TreeLeftRotation)
    end

(** Creates a transformation that transforms a tree in such a way that it will
    move a single leaf from its right subtree to its left subtree. *)
let transfer_right_to_left_subtree_single (tree : binary_tree) :
    tree_transformation =
  match tree with
  | Leaf -> failwith "Cannot transfer between subtrees of leaf"
  | Node (_, r) ->
      TreeSequence (TreeRightApply (bring_leaf_to_top_left r), TreeLeftRotation)

(** Creates a transformation that transforms a tree in such a way that it will
    move a given amount of leaves from its right subtree to its left subtree. *)
let rec transfer_right_to_left_subtree (tree : binary_tree) (amount : int) :
    tree_transformation =
  if amount < 0 then
    failwith "Amount must be nonnegative"
  else if amount = 0 then
    TreeIdentity
  else
    let single_transfer = transfer_right_to_left_subtree_single tree in
    let tree' = transform_tree single_transfer tree in
      TreeSequence
        (single_transfer, transfer_right_to_left_subtree tree' (amount - 1))

(** Creates a transformation that transforms a tree in such a way that it will
    move a single leaf from its left subtree to its right subtree. *)
let transfer_left_to_right_subtree_single (tree : binary_tree) :
    tree_transformation =
  match tree with
  | Leaf -> failwith "Cannot transfer between subtrees of leaf"
  | Node (l, _) ->
      TreeSequence
        (TreeLeftApply (bring_leaf_to_top_right l), TreeRightRotation)

(** Creates a transformation that transforms a tree in such a way that it will
    move a given amount of leaves from its left subtree to its right subtree. *)
let rec transfer_left_to_right_subtree (tree : binary_tree) (amount : int) :
    tree_transformation =
  if amount < 0 then
    failwith "Amount must be nonnegative"
  else if amount = 0 then
    TreeIdentity
  else
    let single_transfer = transfer_left_to_right_subtree_single tree in
    let tree' = transform_tree single_transfer tree in
      TreeSequence
        (single_transfer, transfer_left_to_right_subtree tree' (amount - 1))

(** Creates a tree transformation that transforms [tree0] in such a way that
    its shape matches that of [tree1]. *)
let rec reshape_tree (tree0 : binary_tree) (tree1 : binary_tree) :
    tree_transformation =
  if tree_size tree0 <> tree_size tree1 then
    failwith "Trees must be of the same size to match shape"
  else
    match (tree0, tree1) with
    | Leaf, Leaf -> TreeIdentity
    | Node (l0, _), Node (l1, r1) -> begin
        let sl0 = tree_size l0 in
        let sl1 = tree_size l1 in
        let trans =
          if sl0 < sl1 then
            transfer_right_to_left_subtree tree0 (sl1 - sl0)
          else if sl0 > sl1 then
            transfer_left_to_right_subtree tree0 (sl0 - sl1)
          else
            TreeIdentity
        in
        let tree0' = transform_tree trans tree0 in
          match tree0' with
          | Leaf -> failwith "Cannot obtain leaf by applying transfer"
          | Node (l0', r0') ->
              let l_trans, r_trans =
                (reshape_tree l0' l1, reshape_tree r0' r1)
              in
                TreeSequence (trans, TreeBothApply (l_trans, r_trans))
      end
    | _ -> failwith "Mismatching trees"

(** Given a tree and two paths, creates a transformation that brings the nodes
    at these two paths to the same depth, and returns the new locations of
    these nodes along with the transformation. *)
let rec bring_lower_path_to_depth_of_higher (tree : binary_tree)
    (source_path : bool list) (target_path : bool list) :
    tree_transformation * bool list * bool list =
  if List.length source_path = List.length target_path then
    (TreeIdentity, source_path, target_path)
  else if List.length source_path < List.length target_path then
    bring_lower_path_to_depth_of_higher tree target_path source_path
  else
    match List.rev source_path with
    | last :: prev :: grandparent_path_rev -> begin
        let grandparent_path = List.rev grandparent_path_rev in
        let parent_path = grandparent_path @ [prev] in
        let cur_trans, source_path', target_path' =
          begin
            match (prev, last) with
            | false, false -> begin
                if target_path = grandparent_path @ [true] then
                  ( tree_path_apply parent_path TreeCommute,
                    parent_path @ [true],
                    target_path )
                else
                  ( tree_path_apply grandparent_path TreeRightRotation,
                    grandparent_path @ [false],
                    target_path )
              end
            | false, true -> begin
                if target_path = grandparent_path @ [true] then
                  ( tree_path_apply grandparent_path TreeRightRotation,
                    grandparent_path @ [true; false],
                    grandparent_path @ [true; true] )
                else
                  ( tree_path_apply parent_path TreeCommute,
                    parent_path @ [false],
                    target_path )
              end
            | true, false -> begin
                if target_path = grandparent_path @ [false] then
                  ( tree_path_apply grandparent_path TreeLeftRotation,
                    grandparent_path @ [false; true],
                    grandparent_path @ [false; false] )
                else
                  ( tree_path_apply parent_path TreeCommute,
                    parent_path @ [true],
                    target_path )
              end
            | true, true -> begin
                if target_path = grandparent_path @ [false] then
                  ( tree_path_apply parent_path TreeCommute,
                    parent_path @ [false],
                    target_path )
                else
                  ( tree_path_apply grandparent_path TreeLeftRotation,
                    grandparent_path @ [true],
                    target_path )
              end
          end
        in
        let tree' = transform_tree cur_trans tree in
        let next_trans, source_path'', target_path'' =
          bring_lower_path_to_depth_of_higher tree' source_path' target_path'
        in
          (TreeSequence (cur_trans, next_trans), source_path'', target_path'')
      end
    | _ -> failwith "Expected current node to have a depth of at least 2"

(** Given two paths of equal length, creates a transformation that moves the
    node at the source path to almost the same path as the target path, except
    the first place where the two paths differ is kept. *)
let rec almost_swap_two_equal_length_paths (source_path : bool list)
    (target_path : bool list) : tree_transformation =
  if List.length source_path <> List.length target_path then
    failwith "Expected paths to have equal length"
  else
    let paths_equal =
      List.map (fun (a, b) -> a = b) (List.combine source_path target_path)
    in
      if List.for_all (fun x -> x) paths_equal then
        failwith "Expected paths to be not completely equal"
      else if List.for_all (fun x -> x) (List.tl (List.rev paths_equal)) then
        TreeIdentity
      else
        let last_equal = List.hd (List.rev paths_equal) in
        let source_path' = List.rev (List.tl (List.rev source_path)) in
        let target_path' = List.rev (List.tl (List.rev target_path)) in
        let cur_trans =
          if last_equal then
            TreeIdentity
          else
            tree_path_apply source_path' TreeCommute
        in
        let rest_trans =
          almost_swap_two_equal_length_paths source_path' target_path'
        in
          TreeSequence (cur_trans, rest_trans)

(** Creates a transformation that swaps the subtrees of the given tree located
    at two different paths. *)
let move_path (tree : binary_tree) (source_path : bool list)
    (target_path : bool list) : tree_transformation =
  let equalizing_depth, source_path, target_path =
    bring_lower_path_to_depth_of_higher tree source_path target_path
  in
  let swapping =
    if source_path = target_path then
      TreeIdentity
    else begin
      let almost_swapping =
        almost_swap_two_equal_length_paths source_path target_path
      in
      let paths_equal =
        List.map (fun (a, b) -> a = b) (List.combine source_path target_path)
      in
      let first_diff_index = list_index ( = ) paths_equal false in
      let top_path, bottom_path =
        list_split_at_i target_path first_diff_index
      in
      let final_swap =
        tree_path_apply top_path (TreeConditionalCommute (List.tl bottom_path))
      in
        TreeSequence
          ( almost_swapping,
            TreeSequence
              (final_swap, tree_transformation_inverse almost_swapping) )
    end
  in
    TreeSequence
      ( equalizing_depth,
        TreeSequence (swapping, tree_transformation_inverse equalizing_depth)
      )

(** Given a [source] valued tree and a [target] valued tree, assumed to be of
    the same shape, as well as a value [x], creates a tree transformation that
    moves the leaf containing [x] in [source] to the position where [x] occurs
    in [target]. *)
let move_value (source : 'a valued_binary_tree)
    (target : 'a valued_binary_tree) (x : 'a) : tree_transformation =
  match (tree_get_path source x, tree_get_path target x) with
  | Some source_path, Some target_path ->
      move_path (tree_valued_to_normal source) source_path target_path
  | _ -> failwith "Value not found in target tree"

(** Given a [source] valued tree and a [target] valued tree, assumed to be of
    the same shape, as well as a list [values], creates a tree transformation
    that, for each [x] in [values], moves the leaf containing [x] in [source]
    to the position where [x] occurs in [target]. *)
let rec move_values (source : 'a valued_binary_tree)
    (target : 'a valued_binary_tree) (values : 'a list) : tree_transformation =
  match values with
  | [] -> TreeIdentity
  | x :: values' -> begin
      let cur_trans = move_value source target x in
      let source' = transform_valued_tree cur_trans source in
      let rest_trans = move_values source' target values' in
        TreeSequence (cur_trans, rest_trans)
    end

(** Creates a tree transformation that transforms [tree0] in such a way that
    becomes equal to [tree1], having the same shape and the same values in the
    same positions. *)
let reshape_valued_tree (source : 'a valued_binary_tree)
    (target : 'a valued_binary_tree) =
  let unvalued_reshape =
    reshape_tree (tree_valued_to_normal source) (tree_valued_to_normal target)
  in
  let source = transform_valued_tree unvalued_reshape source in
  let values = tree_values source in
    if
      List.sort Stdlib.compare values
      <> List.sort Stdlib.compare (tree_values target)
    then
      failwith "Values in source and target tree must be the same"
    else
      TreeSequence (unvalued_reshape, move_values source target values)
