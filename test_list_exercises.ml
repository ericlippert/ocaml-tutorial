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

(* main *)

let () =  
    test_many "length" length standard_cases [7; 4; 2; 1; 0] string_of_int
    ;;
    


