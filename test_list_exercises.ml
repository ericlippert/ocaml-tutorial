(* tests for OCaml tutorial list exercises *)

open List_exercises;;
open Printing;;
open Test_exercises;;

let standard_cases = 
    [ 
        [1; 1; 1; 2; 2; 3; 4]; 
        [1; 2; 3; 4]; 
        [1; 2]; 
        [1]; 
        []
    ];;

let test_last () =
    test_many "last" last standard_cases [Some 4; Some 4; Some 2; Some 1; None] string_of_int_option;;

let test_last_but_one () =
    test_many "last_but_one" last_but_one standard_cases [Some 3; Some 3; Some 1; None; None] string_of_int_option;;

let test_last_two () =
    test_many "last_two" last_two standard_cases [Some (3, 4); Some (3, 4); Some (1, 2); None; None] string_of_int_int_tuple_option;;

let test_length () =
    test_many "length" length standard_cases [7; 4; 2; 1; 0] string_of_int;;
    
let test_at_0 () =
    test_many "at 0" (at 0) standard_cases [Some 1; Some 1; Some 1; Some 1; None] string_of_int_option;;

let test_at_1 () =
    test_many "at 1" (at 1) standard_cases [Some 1; Some 2; Some 2; None; None] string_of_int_option;;
    
let test_at_2 () =
    test_many "at 2" (at 2) standard_cases [Some 1; Some 3; None; None; None] string_of_int_option;;

let test_list () =
    test_last ();
    test_last_but_one ();
    test_last_two ();
    test_at_0 ();
    test_at_1 ();
    test_at_2 ();
    
    test_length ();
    ;;
