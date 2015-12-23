(* tests for OCaml tutorial arithmetic exercises *)

open Arithmetic_exercises;;
open List_exercises;;
open Printing;;
open Test_exercises;;

let test_is_prime () =
    test_many "is_prime" is_prime (range 0 6) [false; false; true; true; false; true; false ] string_of_bool;;

let test_arithmetic () =
    test_is_prime ();
    ;;
