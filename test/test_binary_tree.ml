open Qunity
open Binary_tree

let all_passed = ref true

let test_tree_balancing (testname : string) (tree : binary_tree)
    (expected : binary_tree) =
  Printf.printf "%s: %!" testname;
  try
    let balanced = balance_tree_apply tree in
      if balanced = expected then
        Printf.printf "passed\n%!"
      else begin
        Printf.printf "FAILED\n%!";
        all_passed := false
      end
  with
  | Failure err -> begin
      Printf.printf "FAILED\nWith error: %s\n" err;
      all_passed := false
    end

let () =
  begin
    Printf.printf
      "\n\
       =========================\n\
       RUNNING BINARY TREE TESTS\n\
       =========================\n\
       %!";

    test_tree_balancing "balance1" Leaf Leaf;

    test_tree_balancing "balance2" (Node (Leaf, Leaf)) (Node (Leaf, Leaf));

    test_tree_balancing "balance3"
      (Node (Node (Leaf, Leaf), Leaf))
      (Node (Node (Leaf, Leaf), Leaf));

    test_tree_balancing "balance4"
      (Node (Leaf, Node (Leaf, Leaf)))
      (Node (Node (Leaf, Leaf), Leaf));

    test_tree_balancing "balance5"
      (Node (Node (Leaf, Leaf), Node (Leaf, Leaf)))
      (Node (Node (Leaf, Leaf), Node (Leaf, Leaf)));

    test_tree_balancing "balance6"
      (Node (Node (Node (Leaf, Leaf), Leaf), Node (Leaf, Leaf)))
      (Node (Node (Node (Leaf, Leaf), Leaf), Node (Leaf, Leaf)));

    test_tree_balancing "balance7"
      (Node (Node (Leaf, Node (Leaf, Leaf)), Node (Leaf, Leaf)))
      (Node (Node (Node (Leaf, Leaf), Leaf), Node (Leaf, Leaf)));

    test_tree_balancing "balance8"
      (Node (Node (Leaf, Leaf), Node (Leaf, Node (Leaf, Leaf))))
      (Node (Node (Node (Leaf, Leaf), Leaf), Node (Leaf, Leaf)));

    test_tree_balancing "balance9"
      (Node (Node (Node (Leaf, Leaf), Node (Leaf, Leaf)), Leaf))
      (Node (Node (Node (Leaf, Leaf), Leaf), Node (Leaf, Leaf)));

    test_tree_balancing "balance10"
      (Node
         ( Node
             ( Node
                 ( Node
                     ( Node (Node (Node (Node (Leaf, Leaf), Leaf), Leaf), Leaf),
                       Leaf ),
                   Leaf ),
               Leaf ),
           Leaf ))
      (Node
         ( Node (Node (Node (Leaf, Leaf), Leaf), Node (Leaf, Leaf)),
           Node (Node (Leaf, Leaf), Node (Leaf, Leaf)) ));

    test_tree_balancing "balance11"
      (Node
         ( Leaf,
           Node
             ( Leaf,
               Node
                 ( Leaf,
                   Node
                     ( Leaf,
                       Node (Leaf, Node (Leaf, Node (Leaf, Node (Leaf, Leaf))))
                     ) ) ) ))
      (Node
         ( Node (Node (Node (Leaf, Leaf), Leaf), Node (Leaf, Leaf)),
           Node (Node (Leaf, Leaf), Node (Leaf, Leaf)) ));

    if !all_passed then
      Printf.printf "\nALL BINARY TREE TESTS PASSED\n\n"
    else begin
      Printf.printf "\nSOME BINARY TREE TESTS FAILED\n\n";
      exit 1
    end
  end
