(* tests for OCaml tutorial arithmetic exercises *)

open Arithmetic_exercises;;
open List_exercises;;
open Printing;;
open Test_exercises;;

let test_is_prime () =
    test_many "is_prime" is_prime (range 0 6) [false; false; true; true; false; true; false ] string_of_bool;;
    
let test_phi () =
    test_many "phi" phi (range 0 10) [0; 0; 0; 1; 1; 3; 1; 5; 3; 5; 3] string_of_int;;
    
let test_factors () =
    test_many "factors" factors (range 1 12) [[1]; [2]; [3]; [2; 2]; [5]; [2; 3]; [7]; [2; 2; 2]; [3; 3]; [2; 5]; [11]; [2; 2; 3]] string_of_int_list;;

let test_factorization () =
    test_many "factorization" factorization (range 1 12) 
        [[(1, 1)]; [(1, 2)]; [(1, 3)]; [(2, 2)]; [(1, 5)]; [(1, 2); (1,3)]; [(1, 7)]; [(3, 2)]; [(2, 3)]; [(1, 2); (1, 5)]; [(1, 11)]; [(2, 2); (1, 3)]] 
        string_of_int_int_tuple_list;;

let test_arithmetic () =
    test_is_prime ();
    test_phi ();
    test_factors ();
    test_factorization ();
    ;;
