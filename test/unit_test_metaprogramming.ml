open Qunity
open Util
open Syntax
open Metaprogramming
open Testing_utils

let test_typedef () =
  let def = Typedef ("MyType", [], Qunit) in
  let dm = update_defmap_with_def dm_empty def in
  Alcotest.(check string) "typedef" "MyType" (name_of_def def);
  Alcotest.(check bool) "typedef in defs" true (StringMap.mem "MyType" dm.defs)

let test_typedef_variant () =
  let def = TypedefVariant ("MyVariant", [], [("A", Qunit); ("B", Qunit)]) in
  let dm = update_defmap_with_def dm_empty def in
  Alcotest.(check string) "typedef_variant" "MyVariant" (name_of_def def);
  Alcotest.(check bool) "typedef_variant in defs" true (StringMap.mem "MyVariant" dm.defs);
  Alcotest.(check bool) "constructor A in constructors" true (StringMap.mem "A" dm.constructors);
  Alcotest.(check bool) "constructor B in constructors" true (StringMap.mem "B" dm.constructors)

let test_exprdef () =
  let def = Exprdef ("myExpr", [], Qunit, Null) in
  let dm = update_defmap_with_def dm_empty def in
  Alcotest.(check string) "exprdef" "myExpr" (name_of_def def);
  Alcotest.(check bool) "exprdef in defs" true (StringMap.mem "myExpr" dm.defs)

let test_progdef () =
  let def = Progdef ("myProg", [], Qunit, Qunit, Lambda (Var "x", Var "x")) in
  let dm = update_defmap_with_def dm_empty def in
  Alcotest.(check string) "progdef" "myProg" (name_of_def def);
  Alcotest.(check bool) "progdef in defs" true (StringMap.mem "myProg" dm.defs)

let test_realdef () =
  let def = Realdef ("myReal", [], Const 1) in
  let dm = update_defmap_with_def dm_empty def in
  Alcotest.(check string) "realdef" "myReal" (name_of_def def);
  Alcotest.(check bool) "realdef in defs" true (StringMap.mem "myReal" dm.defs)

let () = Alcotest.run "metaprogramming" [
  "update_defmap_with_def", [
    Alcotest.test_case "test_typedef" `Quick test_typedef;
    Alcotest.test_case "test_typedef_variant" `Quick test_typedef_variant;
    Alcotest.test_case "test_exprdef" `Quick test_exprdef;
    Alcotest.test_case "test_progdef" `Quick test_progdef;
    Alcotest.test_case "test_realdef" `Quick test_realdef;
  ]
]
