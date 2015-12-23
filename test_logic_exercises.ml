(* tests for OCaml tutorial arithmetic exercises *)

open Logic_exercises;;
open Printing;;
open Test_exercises;;

let test_gray () = 
    test_single "gray" gray 3 ["000"; "001"; "011"; "010"; "110"; "111"; "101"; "100"] string_of_string_list;;

let test_logic () =
    test_gray ();
    ;;
