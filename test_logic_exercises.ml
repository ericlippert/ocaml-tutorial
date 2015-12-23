(* tests for OCaml tutorial arithmetic exercises *)

open Logic_exercises;;
open Printing;;
open Test_exercises;;

let test_gray () = 
    test_single "gray" gray 3 ["000"; "001"; "011"; "010"; "110"; "111"; "101"; "100"] string_of_string_list;;

let test_huffman () = 
    test_single 
        "huffman" 
        huffman 
        [ ("a", 45); ("b", 13); ("c", 12); ("d", 16); ("e", 9); ("f", 5)] 
        [("a", "0"); ("c", "100"); ("b", "101"); ("f", "1100"); ("e", "1101"); ("d", "111")]
        string_of_string_string_tuple_list;;

let test_logic () =
    test_gray ();
    test_huffman ();
    ;;
