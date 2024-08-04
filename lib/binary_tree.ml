open Util

type binary_tree = Leaf | Node of binary_tree * binary_tree

type tree_transformation =
  | TreeIdentity
  | TreeLeftRotation
  | TreeRightRotation
  | TreeLeftApply of tree_transformation
  | TreeRightApply of tree_transformation
  | TreeSequence of tree_transformation * tree_transformation

let rec tree_size (tree : binary_tree) : int =
  match tree with
  | Leaf -> 1
  | Node (l, r) -> tree_size l + tree_size r

let rec transform_tree (trans : tree_transformation) (tree : binary_tree) :
    binary_tree =
  match tree with
  | Leaf ->
      if trans = TreeIdentity then
        tree
      else
        failwith "Cannot transform leaf"
  | Node (l, r) -> begin
      match trans with
      | TreeIdentity -> tree
      | TreeLeftRotation -> begin
          match r with
          | Leaf ->
              failwith "Cannot apply left rotation when right child is leaf"
          | Node (rl, rr) -> Node (Node (l, rl), rr)
        end
      | TreeRightRotation -> begin
          match l with
          | Leaf ->
              failwith "Cannot apply right rotation when left child is leaf"
          | Node (ll, lr) -> Node (ll, Node (lr, r))
        end
      | TreeLeftApply trans' -> Node (transform_tree trans' l, r)
      | TreeRightApply trans' -> Node (l, transform_tree trans' r)
      | TreeSequence (trans0, trans1) ->
          transform_tree trans1 (transform_tree trans0 tree)
    end

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

let transfer_right_to_left_subtree_single (tree : binary_tree) :
    tree_transformation =
  match tree with
  | Leaf -> failwith "Cannot transfer between subtrees of leaf"
  | Node (_, r) ->
      TreeSequence (TreeRightApply (bring_leaf_to_top_left r), TreeLeftRotation)

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

let transfer_left_to_right_subtree_single (tree : binary_tree) :
    tree_transformation =
  match tree with
  | Leaf -> failwith "Cannot transfer between subtrees of leaf"
  | Node (l, _) ->
      TreeSequence
        (TreeLeftApply (bring_leaf_to_top_right l), TreeRightRotation)

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

let rec balance_tree (tree : binary_tree) : tree_transformation =
  match tree with
  | Leaf -> TreeIdentity
  | Node (l, r) -> begin
      let sl = tree_size l in
      let sr = tree_size r in
      let goal_sl = complete_binary_left_subtree (sl + sr) in
      let transfer =
        begin
          if sl < goal_sl then
            transfer_right_to_left_subtree tree (goal_sl - sl)
          else if sl > goal_sl then
            transfer_left_to_right_subtree tree (sl - goal_sl)
          else
            TreeIdentity
        end
      in
      let tree' = transform_tree transfer tree in
        match tree' with
        | Leaf -> failwith "Impossible to get leaf by transferring"
        | Node (l', r') -> begin
            TreeSequence
              ( transfer,
                TreeSequence
                  ( TreeLeftApply (balance_tree l'),
                    TreeRightApply (balance_tree r') ) )
          end
    end

let balance_tree_apply (tree : binary_tree) : binary_tree =
  transform_tree (balance_tree tree) tree
