(* tests for OCaml tutorial arithmetic exercises *)

open Arithmetic_exercises;;
open List_exercises;;
open Printing;;
open Test_exercises;;

let test_is_prime () =
    test_many "is_prime" is_prime (range 0 6) [false; false; true; true; false; true; false ] string_of_bool;;
    
let test_phi () =
    test_many "phi" phi (range 1 10) [1; 1; 2; 2; 4; 2; 6; 4; 6; 4] string_of_int;;
    
let test_phi_improved () =
    test_many "phi_improved" phi_improved (range 1 10) [1; 1; 2; 2; 4; 2; 6; 4; 6; 4] string_of_int;;

let test_factors () =
    test_many "factors" factors (range 1 12) [[1]; [2]; [3]; [2; 2]; [5]; [2; 3]; [7]; [2; 2; 2]; [3; 3]; [2; 5]; [11]; [2; 2; 3]] string_of_int_list;;

let test_factorization () =
    test_many "factorization" factorization (range 1 12) 
        [[(1, 1)]; [(1, 2)]; [(1, 3)]; [(2, 2)]; [(1, 5)]; [(1, 2); (1,3)]; [(1, 7)]; [(3, 2)]; [(2, 3)]; [(1, 2); (1, 5)]; [(1, 11)]; [(2, 2); (1, 3)]] 
        string_of_int_int_tuple_list;;

let test_power0 () =
    test_many "power0" (fun x -> power x 0) (range 1 5) [1; 1; 1; 1; 1] string_of_int;;
    
let test_power1 () =
    test_many "power1" (fun x -> power x 1) (range 0 5) [0; 1; 2; 3; 4; 5] string_of_int;;

let test_power3 () =
    test_many "power3" (fun x -> power x 3) (range 0 5) [0; 1; 8; 27; 64; 125] string_of_int;;

let test_all_primes () =
    test_single "all_primes" (all_primes 10) 23 [11; 13; 17; 19; 23] string_of_int_list;;
    
let test_goldbach () =
    test_many "goldbach" goldbach [4; 6; 8; 10; 12; 14] [(2, 2); (3, 3); (3, 5); (3, 7); (5, 7); (3, 11)] string_of_int_int_tuple;;

let test_arithmetic () =
    test_is_prime ();
    test_phi ();
    test_phi_improved ();
    test_factors ();
    test_factorization ();
    test_power0 ();
    test_power1 ();
    test_power3 ();
    test_all_primes ();
    test_goldbach ();
    ;;
