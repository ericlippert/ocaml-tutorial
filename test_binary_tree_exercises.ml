(* tests for OCaml tutorial binary tree exercises *)

open Binary_tree_exercises;;
open Printing;;
open Test_exercises;;

let string_of_int_tree_list = string_of_a_list (string_of_binary_tree string_of_int);;

let test_cbal () =
    test_single "cbal" cbal 4 
[Node (0, Node (0, Empty, Empty), Node (0, Empty, Node (0, Empty, Empty)));
 Node (0, Node (0, Empty, Empty), Node (0, Node (0, Empty, Empty), Empty));
 Node (0, Node (0, Empty, Node (0, Empty, Empty)), Node (0, Empty, Empty));
 Node (0, Node (0, Node (0, Empty, Empty), Empty), Node (0, Empty, Empty))]    
string_of_int_tree_list;;

let test_binary_trees () =
    test_cbal ();
    ;;
