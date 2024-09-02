open Qunity
open Util
open Binary_tree
open Testing_utils

let example_tree =
  Node
    ( Node (Node (Leaf, Leaf), Node (Node (Leaf, Leaf), Leaf)),
      Node (Node (Leaf, Node (Leaf, Node (Leaf, Leaf))), Node (Leaf, Leaf)) )

let tree_pair_gen =
  QCheck.Gen.(
    fun (st : Random.State.t) ->
      sized_size small_nat
        (fun (n : int) ->
          let n = n + 1 in
          let single_tree_gen (shuffle : bool) =
            (fix
               (fun
                 (self : int list -> int valued_binary_tree t)
                 (l : int list)
               ->
                 match l with
                 | [x] -> fun _ -> ValuedLeaf x
                 | _ ->
                     let i = int_range 1 (List.length l - 1) st in
                     let l0, l1 = list_split_at_i l i in
                       map2
                         (fun (a : int valued_binary_tree)
                              (b : int valued_binary_tree) ->
                           ValuedNode (a, b))
                         (self l0) (self l1)))
              (if shuffle then shuffle_l (range n) st else range n)
          in
            pair (single_tree_gen false) (single_tree_gen true))
        st)

let string_of_valued_tree_pair
    ((a : int valued_binary_tree), (b : int valued_binary_tree)) =
  "source: "
  ^ string_of_valued_tree string_of_int a
  ^ "\n" ^ "target: "
  ^ string_of_valued_tree string_of_int b

let arbitrary_tree_pair =
  QCheck.make tree_pair_gen ~print:string_of_valued_tree_pair

let () =
  (* Printf.printf "Examples of generated tree pairs:\n";
     List.iter
       (fun ((a : int valued_binary_tree), (b : int valued_binary_tree)) ->
         Printf.printf "%s\n\n" (string_of_valued_tree_pair (a, b)))
       (QCheck.Gen.generate ~n:20 tree_pair_gen); *)
  Alcotest.run "binary_tree"
    [
      ( "tree_size",
        [
          test "tree_size_1" (fun () ->
              Alcotest.(check int "") 1 (tree_size Leaf));
          test "tree_size_2" (fun () ->
              Alcotest.(check int "") 2 (tree_size (Node (Leaf, Leaf))));
          test "tree_size_3" (fun () ->
              Alcotest.(check int "")
                5
                (tree_size
                   (Node (Node (Leaf, Node (Leaf, Leaf)), Node (Leaf, Leaf)))));
          test "tree_size_4" (fun () ->
              Alcotest.(check int "") 11 (tree_size example_tree));
        ] );
      ( "tree_height",
        [
          test "tree_height_1" (fun () ->
              Alcotest.(check int "") 0 (tree_height Leaf));
          test "tree_height_2" (fun () ->
              Alcotest.(check int "") 1 (tree_height (Node (Leaf, Leaf))));
          test "tree_height_3" (fun () ->
              Alcotest.(check int "")
                3
                (tree_height
                   (Node (Node (Leaf, Node (Leaf, Leaf)), Node (Leaf, Leaf)))));
          test "tree_height_3" (fun () ->
              Alcotest.(check int "") 5 (tree_height example_tree));
        ] );
      ( "tree_multiply",
        [
          test "tree_multiply_1" (fun () ->
              Alcotest.(check binary_tree "")
                example_tree
                (tree_multiply Leaf [example_tree]));
          test "tree_multiply_2" (fun () ->
              Alcotest.(check binary_tree "")
                example_tree
                (tree_multiply example_tree
                   (list_constant Leaf (tree_size example_tree))));
          test "tree_multiply_3" (fun () ->
              Alcotest.(check binary_tree "")
                (Node (Node (Leaf, Leaf), Node (Leaf, Node (Leaf, Leaf))))
                (tree_multiply
                   (Node (Leaf, Leaf))
                   [Node (Leaf, Leaf); Node (Leaf, Node (Leaf, Leaf))]));
        ] );
      ( "reshape_valued_tree",
        [
          QCheck_alcotest.to_alcotest
            (QCheck.Test.make ~count:1000 ~name:"reshape_valued_tree"
               arbitrary_tree_pair (fun (source, target) ->
                 transform_valued_tree
                   (reshape_valued_tree source target)
                   source
                 = target));
        ] );
    ]
