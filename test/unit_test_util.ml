open Qunity
open Util
open Syntax
open Testing_utils

let () =
  Alcotest.run "util"
    [
      ( "all_or_nothing",
        [
          test "all_1" (fun () ->
              Alcotest.(check (option (list int)) "")
                (Some []) (all_or_nothing []));
          test "all_2" (fun () ->
              Alcotest.(check (option (list int)) "")
                (Some [1; 2; 3; 4])
                (all_or_nothing [Some 1; Some 2; Some 3; Some 4]));
          test "all_3" (fun () ->
              Alcotest.(check (option (list expr)) "")
                (Some [Null; Qpair (Null, Null); bit0; bit1])
                (all_or_nothing
                   [Some Null; Some (Qpair (Null, Null)); Some bit0; Some bit1]));
          test "nothing_1" (fun () ->
              Alcotest.(check (option (list int)) "")
                None (all_or_nothing [None]));
          test "nothing_2" (fun () ->
              Alcotest.(check (option (list expr)) "")
                None
                (all_or_nothing [Some bit0; Some Null; None; Some bit1]));
        ] );
      ( "map_all_or_nothing",
        [
          test "map_all_1" (fun () ->
              Alcotest.(check (option context) "")
                (Some StringMap.empty)
                (map_all_or_nothing StringMap.empty));
          test "map_all_2" (fun () ->
              Alcotest.(check (option context) "")
                (Some (StringMap.singleton "x" bit))
                (map_all_or_nothing (StringMap.singleton "x" (Some bit))));
          test "map_all_3" (fun () ->
              Alcotest.(check (option context) "")
                (Some (StringMap.of_list [("x", bit); ("y", Qunit)]))
                (map_all_or_nothing
                   (StringMap.of_list [("y", Some Qunit); ("x", Some bit)])));
          test "map_nothing_1" (fun () ->
              Alcotest.(check (option context) "")
                None
                (map_all_or_nothing (StringMap.singleton "x" None)));
          test "map_nothing_2" (fun () ->
              Alcotest.(check (option context) "")
                None
                (map_all_or_nothing
                   (StringMap.of_list [("y", Some Qunit); ("x", None)])));
        ] );
      ( "fresh_string",
        [
          test "fresh_string_1" (fun () ->
              Alcotest.(check string "")
                "$0"
                (fresh_string "$" StringSet.empty));
          test "fresh_string_2" (fun () ->
              Alcotest.(check string "")
                "$0"
                (fresh_string "$" (StringSet.singleton "foo")));
          test "fresh_string_3" (fun () ->
              Alcotest.(check string "")
                "$1"
                (fresh_string "$" (StringSet.singleton "$0")));
          test "fresh_string_4" (fun () ->
              Alcotest.(check string "")
                "$2"
                (fresh_string "$" (StringSet.of_list ["$0"; "$1"])));
          test "fresh_string_5" (fun () ->
              Alcotest.(check string "")
                "_4"
                (fresh_string "_"
                   (StringSet.of_list
                      ["_1"; "foo"; "_0"; "bar"; "_"; "_6"; "_2"; "_3"])));
        ] );
      ( "fresh_int_list",
        [
          test "fresh_int_list_1" (fun () ->
              Alcotest.(check (pair (list int) int_set) "")
                ([], IntSet.empty)
                (fresh_int_list IntSet.empty 0));
          test "fresh_int_list_2" (fun () ->
              Alcotest.(check (pair (list int) int_set) "")
                ([0; 1; 2; 3; 4], IntSet.of_list [0; 1; 2; 3; 4])
                (fresh_int_list IntSet.empty 5));
          test "fresh_int_list_3" (fun () ->
              Alcotest.(check (pair (list int) int_set) "")
                ([1; 3; 5; 6; 7], IntSet.of_list [0; 1; 2; 3; 4; 5; 6; 7])
                (fresh_int_list (IntSet.of_list [0; 2; 4]) 5));
        ] );
      ( "fresh_int_lists",
        [
          test "fresh_int_lists_1" (fun () ->
              Alcotest.(check (pair (list (list int)) int_set) "")
                ([], IntSet.empty)
                (fresh_int_lists IntSet.empty []));
          test "fresh_int_lists_2" (fun () ->
              Alcotest.(check (pair (list (list int)) int_set) "")
                ([[0; 1]; [2; 3; 4]], IntSet.of_list [0; 1; 2; 3; 4])
                (fresh_int_lists IntSet.empty [2; 3]));
          test "fresh_int_lists_3" (fun () ->
              Alcotest.(check (pair (list (list int)) int_set) "")
                ([[1; 3; 5; 6]; [7]], IntSet.of_list [0; 1; 2; 3; 4; 5; 6; 7])
                (fresh_int_lists (IntSet.of_list [0; 2; 4]) [4; 1]));
        ] );
      ( "map_merge",
        [
          test "allow_dup_no_dup" (fun () ->
              Alcotest.(check (optionE context) "")
                (SomeE
                   (StringMap.of_list
                      [
                        ("a", Qunit);
                        ("b", bit);
                        ("x", ProdType (bit, bit));
                        ("y", SumType (Qunit, bit));
                      ]))
                (map_merge true
                   (StringMap.of_list [("a", Qunit); ("b", bit)])
                   (StringMap.of_list
                      [("x", ProdType (bit, bit)); ("y", SumType (Qunit, bit))])));
          test "allow_dup_good_dup" (fun () ->
              Alcotest.(check (optionE context) "")
                (SomeE
                   (StringMap.of_list
                      [("a", Qunit); ("b", ProdType (bit, bit)); ("x", bit)]))
                (map_merge true
                   (StringMap.of_list [("a", Qunit); ("x", bit)])
                   (StringMap.of_list [("b", ProdType (bit, bit)); ("x", bit)])));
          test "allow_dup_bad_dup" (fun () ->
              Alcotest.(check (optionE context) "")
                (NoneE "Value mismatch in map merge")
                (map_merge true
                   (StringMap.of_list [("a", Qunit); ("x", bit)])
                   (StringMap.of_list
                      [("b", ProdType (bit, bit)); ("x", SumType (Qunit, bit))])));
          test "disallow_dup_no_dup" (fun () ->
              Alcotest.(check (optionE context) "")
                (SomeE
                   (StringMap.of_list
                      [
                        ("a", Qunit);
                        ("b", bit);
                        ("x", ProdType (bit, bit));
                        ("y", SumType (Qunit, bit));
                      ]))
                (map_merge false
                   (StringMap.of_list [("a", Qunit); ("b", bit)])
                   (StringMap.of_list
                      [("x", ProdType (bit, bit)); ("y", SumType (Qunit, bit))])));
          test "disallow_dup_good_dup" (fun () ->
              Alcotest.(check (optionE context) "")
                (NoneE "Expected disjoint maps")
                (map_merge false
                   (StringMap.of_list [("a", Qunit); ("x", bit)])
                   (StringMap.of_list [("b", ProdType (bit, bit)); ("x", bit)])));
          test "disallow_dup_bad_dup" (fun () ->
              Alcotest.(check (optionE context) "")
                (NoneE "Expected disjoint maps")
                (map_merge false
                   (StringMap.of_list [("a", Qunit); ("x", bit)])
                   (StringMap.of_list
                      [("b", ProdType (bit, bit)); ("x", SumType (Qunit, bit))])));
        ] );
      ( "map_is_inclusion",
        [
          test "is_inclusion_1" (fun () ->
              Alcotest.(check bool "")
                true
                (map_is_inclusion StringMap.empty
                   (StringMap.of_list [("x", Qunit); ("y", bit)])));
          test "is_inclusion_2" (fun () ->
              Alcotest.(check bool "")
                true
                (map_is_inclusion
                   (StringMap.singleton "x" Qunit)
                   (StringMap.of_list [("x", Qunit); ("y", bit)])));
          test "is_not_inclusion_1" (fun () ->
              Alcotest.(check bool "")
                false
                (map_is_inclusion
                   (StringMap.singleton "a" Qunit)
                   (StringMap.of_list [("x", Qunit); ("y", bit)])));
          test "is_not_inclusion_2" (fun () ->
              Alcotest.(check bool "")
                false
                (map_is_inclusion
                   (StringMap.of_list [("x", Qunit); ("y", bit); ("a", bit)])
                   (StringMap.of_list [("x", Qunit); ("y", bit)])));
          test "is_not_inclusion_3" (fun () ->
              Alcotest.(check bool "")
                false
                (map_is_inclusion
                   (StringMap.of_list [("x", Qunit); ("y", Qunit)])
                   (StringMap.of_list [("x", Qunit); ("y", bit)])));
        ] );
      ( "map_dom",
        [
          test "map_dom_1" (fun () ->
              Alcotest.(check string_set "")
                StringSet.empty (map_dom StringMap.empty));
          test "map_dom_2" (fun () ->
              Alcotest.(check string_set "")
                (StringSet.of_list ["a"; "b"; "c"])
                (map_dom
                   (StringMap.of_list
                      [("c", Qunit); ("a", bit); ("b", ProdType (bit, bit))])));
        ] );
      ( "map_restriction",
        [
          test "map_restriction_1" (fun () ->
              Alcotest.(check context "")
                StringMap.empty
                (map_restriction
                   (StringMap.singleton "x" Qunit)
                   StringSet.empty));
          test "map_restriction_2" (fun () ->
              Alcotest.(check context "")
                (StringMap.singleton "x" Qunit)
                (map_restriction
                   (StringMap.singleton "x" Qunit)
                   (StringSet.of_list ["x"; "y"])));
        ] );
      ( "map_exclusion",
        [
          test "map_exclusion_1" (fun () ->
              Alcotest.(check context "")
                StringMap.empty
                (map_exclusion
                   (StringMap.singleton "x" Qunit)
                   (StringSet.singleton "x")));
          test "map_exclusion_2" (fun () ->
              Alcotest.(check context "")
                (StringMap.singleton "x" Qunit)
                (map_exclusion
                   (StringMap.of_list
                      [("x", Qunit); ("y", bit); ("z", ProdType (bit, bit))])
                   (StringSet.of_list ["y"; "z"])));
        ] );
      ( "int_map_find_or_keep",
        [
          test "find" (fun () ->
              Alcotest.(check int "")
                5
                (int_map_find_or_keep
                   (IntMap.of_list [(0, 3); (1, 5); (2, 4)])
                   1));
          test "keep" (fun () ->
              Alcotest.(check int "")
                1
                (int_map_find_or_keep
                   (IntMap.of_list [(0, 3); (3, 5); (2, 4)])
                   1));
        ] );
      ( "int_list_intersection",
        [
          test "empty" (fun () ->
              Alcotest.(check (list int) "")
                []
                (int_list_intersection [0; 2; 4; 6; 8] [1; 3; 5; 7; 9]));
          test "subset_1" (fun () ->
              Alcotest.(check (list int) "")
                [5; 3; 1]
                (int_list_intersection [5; 3; 1] [0; 1; 2; 3; 4; 5]));
          test "subset_2" (fun () ->
              Alcotest.(check (list int) "")
                [1; 3; 5]
                (int_list_intersection [0; 1; 2; 3; 4; 5] [5; 3; 1]));
          test "intersection" (fun () ->
              Alcotest.(check (list int) "")
                [7; 2; 3]
                (int_list_intersection [1; 5; 7; 2; 8; 0; 16; 3]
                   [7; 15; 3; 14; 2; 4]));
        ] );
      ( "int_list_diff",
        [
          test "same" (fun () ->
              Alcotest.(check (list int) "")
                []
                (int_list_diff [4; 1; 5; 2; 3] [1; 2; 3; 4; 5]));
          test "subset_1" (fun () ->
              Alcotest.(check (list int) "")
                []
                (int_list_diff [5; 3; 1] [0; 1; 2; 3; 4; 5]));
          test "subset_2" (fun () ->
              Alcotest.(check (list int) "")
                [0; 2; 4]
                (int_list_diff [0; 1; 2; 3; 4; 5] [5; 3; 1]));
          test "diff" (fun () ->
              Alcotest.(check (list int) "")
                [1; 5; 8; 0; 16]
                (int_list_diff [1; 5; 7; 2; 8; 0; 16; 3] [7; 15; 3; 14; 2; 4]));
        ] );
      ( "int_list_max",
        [
          test "empty" (fun () ->
              Alcotest.(check int "") Int.min_int (int_list_max []));
          test "max_1" (fun () ->
              Alcotest.(check int "")
                52
                (int_list_max [0; 5; 2; 52; -3; 24; 36; 3]));
          test "max_2" (fun () ->
              Alcotest.(check int "") (-5) (int_list_max [-10; -6; -5; -12]));
        ] );
      ( "list_index",
        [
          test "list_index_1" (fun () ->
              Alcotest.(check int "") 0 (list_index ( = ) [1; 2; 3; 4; 5] 1));
          test "list_index_2" (fun () ->
              Alcotest.(check int "") 3 (list_index ( = ) [1; 2; 3; 4; 5] 4));
          test "list_index_3" (fun () ->
              Alcotest.(check int "")
                1
                (list_index (StringMap.equal ( = ))
                   [
                     StringMap.of_list [("x", bit0); ("y", bit0)];
                     StringMap.of_list [("y", bit1); ("x", bit0)];
                     StringMap.of_list [("x", bit1); ("y", bit0)];
                     StringMap.of_list [("y", bit1); ("x", bit1)];
                   ]
                   (StringMap.of_list [("x", bit0); ("y", bit1)])));
        ] );
      ( "list_constant",
        [
          test "list_constant_1" (fun () ->
              Alcotest.(check (list int) "") [] (list_constant 10 0));
          test "list_constant_2" (fun () ->
              Alcotest.(check (list bool) "")
                [false; false; false; false; false]
                (list_constant false 5));
        ] );
      ( "list_split_at_i",
        [
          test "list_split_at_i_1" (fun () ->
              Alcotest.(check (pair (list int) (list int)) "")
                ([], [1; 2; 3; 4; 5; 6; 7; 8])
                (list_split_at_i [1; 2; 3; 4; 5; 6; 7; 8] 0));
          test "list_split_at_i_2" (fun () ->
              Alcotest.(check (pair (list int) (list int)) "")
                ([1; 2; 3; 4; 5], [6; 7; 8])
                (list_split_at_i [1; 2; 3; 4; 5; 6; 7; 8] 5));
          test "list_split_at_i_3" (fun () ->
              Alcotest.(check (pair (list int) (list int)) "")
                ([1; 2; 3; 4; 5; 6; 7; 8], [])
                (list_split_at_i [1; 2; 3; 4; 5; 6; 7; 8] 8));
        ] );
      ( "list_split_by_sizes",
        [
          test "list_split_by_sizes_1" (fun () ->
              Alcotest.(check (list (list int)) "")
                []
                (list_split_by_sizes [] []));
          test "list_split_by_sizes_2" (fun () ->
              Alcotest.(check (list (list int)) "")
                [[]]
                (list_split_by_sizes [] [0]));
          test "list_split_by_sizes_3" (fun () ->
              Alcotest.(check (list (list int)) "")
                [[1; 2; 3]; [4; 5]; []]
                (list_split_by_sizes [1; 2; 3; 4; 5] [3; 2; 0]));
          test "list_split_by_sizes_4" (fun () ->
              Alcotest.(check (list (list int)) "")
                [[]; [1; 2; 3]; []; []; [4; 5]; [6]; [7; 8]]
                (list_split_by_sizes [1; 2; 3; 4; 5; 6; 7; 8]
                   [0; 3; 0; 0; 2; 1; 2]));
        ] );
      ( "list_split3",
        [
          test "list_split3_1" (fun () ->
              Alcotest.(check (triple (list int) (list int) (list int)) "")
                ([], [], []) (list_split3 []));
          test "list_split3_1" (fun () ->
              Alcotest.(check (triple (list int) (list string) (list expr)) "")
                ([1; 2; 3], ["foo"; "bar"; "baz"], [Null; bit0; bit1])
                (list_split3
                   [(1, "foo", Null); (2, "bar", bit0); (3, "baz", bit1)]));
        ] );
      ( "range",
        [
          test "range_0" (fun () -> Alcotest.(check (list int) "") [] (range 0));
          test "range_negative" (fun () ->
              Alcotest.(check (list int) "") [] (range (-10)));
          test "range_10" (fun () ->
              Alcotest.(check (list int) "")
                [0; 1; 2; 3; 4; 5; 6; 7; 8; 9]
                (range 10));
        ] );
    ]
