(* helper methods for printing *)

let string_of_a_option string_of_a a_option =
    match a_option with
    | None -> "null"
    | Some a -> string_of_a a;;
    
let string_of_int_option = 
    string_of_a_option string_of_int;;

let string_of_a_b_tuple string_of_a string_of_b a_b_tuple = 
    let (a, b) = a_b_tuple in
    "(" ^ (string_of_a a) ^ ", " ^ (string_of_b b) ^ ")";;

let string_of_string x = x;; 

let string_of_int_int_tuple = 
    string_of_a_b_tuple string_of_int string_of_int;;

let string_of_int_string_tuple = 
    string_of_a_b_tuple string_of_int string_of_string;;

let string_of_a_list string_of_a items =
    let rec string_of_a_list_aux acc xs = 
        match xs with
        | [] -> acc
        | [x] -> acc ^ (string_of_a x)
        | x :: tail -> string_of_a_list_aux (acc ^ (string_of_a x) ^ "; ") tail in
    "[" ^ (string_of_a_list_aux "" items) ^ "]";;
    
let string_of_int_list = 
    string_of_a_list string_of_int;;       

let string_of_int_list_list = 
    string_of_a_list string_of_int_list;;

let string_of_string_list = 
    string_of_a_list string_of_string;;

let string_of_int_string_tuple_list = 
    string_of_a_list string_of_int_string_tuple;;

let string_of_int_int_tuple_list = 
    string_of_a_list string_of_int_int_tuple;;

let string_of_int_list_int_list_tuple = 
    string_of_a_b_tuple string_of_int_list string_of_int_list;;

let string_of_int_list_int_list_tuple_list = 
    string_of_a_list string_of_int_list_int_list_tuple;;

