(* test infrastructure for OCaml tutorial exercises *)

let print_expected_output_on_passed = true;;
    
let on_passed name expected_output string_of_output =
    let header = "Passed: " ^ name in
    let footer = if print_expected_output_on_passed then " " ^ (string_of_output expected_output) else "" in
    print_endline (header ^ footer);;

let on_failed name expected_output observed_output string_of_output =
    print_endline ("Failed: " ^ name);
    print_endline ("Expected output: " ^ (string_of_output expected_output));
    print_endline ("Observed output: " ^ (string_of_output observed_output));
    failwith "stopping testing due to failure";;

let test_single name test_function input expected_output string_of_output = 
    let observed_output = test_function input in
    if observed_output = expected_output then on_passed name expected_output string_of_output 
    else on_failed name expected_output observed_output string_of_output;;

let test_many name test_function inputs expected_outputs string_of_output =
    let run_single input expected_output = test_single name test_function input expected_output string_of_output in
    List.iter2 run_single inputs expected_outputs;;
