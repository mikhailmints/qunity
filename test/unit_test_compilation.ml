open Qunity
open Util
open Syntax
open Compilation
open Testing_utils

let example_tree =
  Node
    ( Node (Node (Leaf, Leaf), Node (Node (Leaf, Leaf), Leaf)),
      Node (Node (Leaf, Node (Leaf, Node (Leaf, Leaf))), Node (Leaf, Leaf)) )

let example_type =
  ProdType
    ( ProdType (SumType (ProdType (bit, bit), bit), bit),
      SumType (SumType (bit, bit), bit) )

let test_circuit_spec (cs : circuit_spec) (in_regs : int list list)
    (additional_used_wires : int list) (settings : instantiation_settings)
    (expected : circuit) (expected_used_wires : int list) =
  begin
    fun () ->
     let used_wires =
       IntSet.of_list (List.flatten in_regs @ additional_used_wires)
     in
     let circ, used_wires = build_circuit cs in_regs used_wires settings in
       Alcotest.(check circuit "check circuit") expected circ;
       Alcotest.(check int_set "check used_wires")
         (IntSet.of_list expected_used_wires)
         used_wires
  end

let settings0 = { reset_flag = true; reset_garb = true; iso = false }

let () =
  Alcotest.run "compilation"
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
      ( "type_size",
        [
          test "void" (fun () -> Alcotest.(check int "") 0 (type_size Void));
          test "qunit" (fun () -> Alcotest.(check int "") 0 (type_size Qunit));
          test "bit" (fun () -> Alcotest.(check int "") 1 (type_size bit));
          test "trit" (fun () ->
              Alcotest.(check int "") 2 (type_size (SumType (Qunit, bit))));
          test "2bit" (fun () ->
              Alcotest.(check int "") 2 (type_size (ProdType (bit, bit))));
          test "big" (fun () ->
              Alcotest.(check int "") 7 (type_size example_type));
        ] );
      ( "context_size",
        [
          test "empty" (fun () ->
              Alcotest.(check int "") 0 (context_size StringMap.empty));
          test "qunit" (fun () ->
              Alcotest.(check int "")
                0
                (context_size (StringMap.singleton "x" Qunit)));
          test "bit" (fun () ->
              Alcotest.(check int "")
                1
                (context_size (StringMap.singleton "x" bit)));
          test "bit_and_trit" (fun () ->
              Alcotest.(check int "")
                3
                (context_size
                   (StringMap.of_list
                      [("x", bit); ("y", SumType (Qunit, bit))])));
          test "bit_and_big" (fun () ->
              Alcotest.(check int "")
                8
                (context_size
                   (StringMap.of_list [("x", bit); ("y", example_type)])));
        ] );
      ( "circuit_nothing",
        [
          test "circuit_nothing"
            (test_circuit_spec circuit_nothing [] [] settings0
               {
                 name = "nothing";
                 in_regs = [];
                 prep_reg = [];
                 out_regs = [];
                 flag_reg = [];
                 garb_reg = [];
                 gate = Identity;
               }
               []);
          test "extra_wires"
            (test_circuit_spec circuit_nothing [] [0; 1; 2; 3] settings0
               {
                 name = "nothing";
                 in_regs = [];
                 prep_reg = [];
                 out_regs = [];
                 flag_reg = [];
                 garb_reg = [];
                 gate = Identity;
               }
               [0; 1; 2; 3]);
        ] );
      ( "circuit_empty",
        [
          test "circuit_empty"
            (test_circuit_spec circuit_empty [] [] settings0
               {
                 name = "empty";
                 in_regs = [];
                 prep_reg = [];
                 out_regs = [[]];
                 flag_reg = [];
                 garb_reg = [];
                 gate = Identity;
               }
               []);
          test "extra_wires"
            (test_circuit_spec circuit_empty [] [1; 3; 5; 7] settings0
               {
                 name = "empty";
                 in_regs = [];
                 prep_reg = [];
                 out_regs = [[]];
                 flag_reg = [];
                 garb_reg = [];
                 gate = Identity;
               }
               [1; 3; 5; 7]);
        ] );
      ( "circuit_prep_reg",
        [
          test "empty"
            (test_circuit_spec (circuit_prep_reg 0) [] [] settings0
               {
                 name = "prep_reg";
                 in_regs = [];
                 prep_reg = [];
                 out_regs = [[]];
                 flag_reg = [];
                 garb_reg = [];
                 gate = Identity;
               }
               []);
          test "nonempty"
            (test_circuit_spec (circuit_prep_reg 5) [] [] settings0
               {
                 name = "prep_reg";
                 in_regs = [];
                 prep_reg = [0; 1; 2; 3; 4];
                 out_regs = [[0; 1; 2; 3; 4]];
                 flag_reg = [];
                 garb_reg = [];
                 gate = Identity;
               }
               [0; 1; 2; 3; 4]);
          test "extra_wires"
            (test_circuit_spec (circuit_prep_reg 5) [] [1; 2; 5; 9] settings0
               {
                 name = "prep_reg";
                 in_regs = [];
                 prep_reg = [0; 3; 4; 6; 7];
                 out_regs = [[0; 3; 4; 6; 7]];
                 flag_reg = [];
                 garb_reg = [];
                 gate = Identity;
               }
               [0; 1; 2; 3; 4; 5; 6; 7; 9]);
        ] );
      ( "circuit_identity",
        [
          test "empty"
            (test_circuit_spec (circuit_identity 0) [[]] [] settings0
               {
                 name = "identity";
                 in_regs = [[]];
                 prep_reg = [];
                 out_regs = [[]];
                 flag_reg = [];
                 garb_reg = [];
                 gate = Identity;
               }
               []);
          test "nonempty"
            (test_circuit_spec (circuit_identity 5)
               [[0; 1; 2; 3; 4]]
               [] settings0
               {
                 name = "identity";
                 in_regs = [[0; 1; 2; 3; 4]];
                 prep_reg = [];
                 out_regs = [[0; 1; 2; 3; 4]];
                 flag_reg = [];
                 garb_reg = [];
                 gate = Identity;
               }
               [0; 1; 2; 3; 4]);
          test "extra_wires"
            (test_circuit_spec (circuit_identity 5)
               [[0; 3; 4; 6; 7]]
               [1; 2; 5; 9] settings0
               {
                 name = "identity";
                 in_regs = [[0; 3; 4; 6; 7]];
                 prep_reg = [];
                 out_regs = [[0; 3; 4; 6; 7]];
                 flag_reg = [];
                 garb_reg = [];
                 gate = Identity;
               }
               [0; 1; 2; 3; 4; 5; 6; 7; 9]);
        ] );
      ( "circuit_annotation",
        [
          test "empty"
            (test_circuit_spec
               (circuit_annotation [] "foo")
               [] [] settings0
               {
                 name = "annotation \"foo\"";
                 in_regs = [];
                 prep_reg = [];
                 out_regs = [];
                 flag_reg = [];
                 garb_reg = [];
                 gate = Annotation ([], "foo");
               }
               []);
          test "nonempty"
            (test_circuit_spec
               (circuit_annotation [3] "foo")
               [[1; 3; 5]]
               [] settings0
               {
                 name = "annotation \"foo\"";
                 in_regs = [[1; 3; 5]];
                 prep_reg = [];
                 out_regs = [[1; 3; 5]];
                 flag_reg = [];
                 garb_reg = [];
                 gate = Annotation ([1; 3; 5], "foo");
               }
               [1; 3; 5]);
          test "multiple"
            (test_circuit_spec
               (circuit_annotation [3; 2] "foo")
               [[1; 3; 5]; [4; 0]]
               [2; 7] settings0
               {
                 name = "annotation \"foo\"";
                 in_regs = [[1; 3; 5]; [4; 0]];
                 prep_reg = [];
                 out_regs = [[1; 3; 5]; [4; 0]];
                 flag_reg = [];
                 garb_reg = [];
                 gate = Annotation ([1; 3; 5; 4; 0], "foo");
               }
               [1; 3; 5; 4; 0; 2; 7]);
        ] );
    ]
