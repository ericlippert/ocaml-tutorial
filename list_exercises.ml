(* list exercises from OCAML tutorial *)

(* return the last member of a list, or None *)
let rec last items =
    match items with
    | [] -> None
    | [item] -> Some item
    | _ :: tail -> last tail;;

(* return the penultimate member of a list, or None *)
let rec last_but_one items =
    match items with
    | [] | [_] -> None
    | item :: _ :: [] -> Some item
    | _ :: tail -> last_but_one tail;;
    
(* return the last two members of a list, possibly None *)
let rec last_two items =
    match items with
    | [] | [_] -> None
    | x :: y :: [] -> Some (x, y)
    | _ :: tail -> last_two tail;;
   
(* return the kth element of a list, indexed from 0, or None *)
(* The exercise calls for this to be indexed from 1, but later 
   exercises specify similar functions that index from zero; this
   is confusing and error-prone. In the interests of consistency,
   this function indexes from zero. *)
let rec at k items =
    match items with
    | [] -> None
    | item :: tail -> 
        if k = 0 then Some item 
        else at (k - 1) tail;;
   
(* return the length of a list *)
let length items =
    let rec length_aux count xs = 
        match xs with
        | [] -> count
        | _ :: tail -> length_aux (count + 1) tail in
    length_aux 0 items;;
   
(* reverse a list *)
let rev items =
    let rec rev_aux acc xs =
        match xs with
        | [] -> acc
        | x :: tail -> rev_aux (x :: acc) tail in
    rev_aux [] items;;

(* return whether a list is a palindrome *) 
let is_palindrome items =   
    items = rev items;;

(* Take a list of nodes, each of which can be a list of nodes or a singleton item. 
   Flatten it into a list of items. *)

module Multinode = struct
    type 'a node =
        | One of 'a
        | Many of 'a node list;;
end

let flatten nodes =
    let rec flatten_aux acc ns = 
        match ns with
        | [] -> acc
        | Multinode.One item :: tail -> flatten_aux (item :: acc) tail
        | Multinode.Many items :: tail -> flatten_aux (flatten_aux acc items) tail in
    rev (flatten_aux [] nodes);;

(* remove consecutive duplicates from a list *) 
(* TODO: this method is not tail recursive. Can we do better? *)
let rec remove_consecutive_duplicates items = 
    match items with
    | [] -> []
    | [x] -> [x]
    | x :: y :: tail ->
        if x = y then remove_consecutive_duplicates (y :: tail) 
        else x :: (remove_consecutive_duplicates (y :: tail));;

(* group consecutive duplicates into sub-lists *)
(* TODO: This checks to see if the group on top of the accumulator
         begins with the given value, and if so, replaces it. 
         There is likely a shorter and more elegant way to do this. *)
let group_consecutive_duplicates items =
    let rec group_consecutive_duplicates_aux acc xs =
    match xs with
    | [] -> acc
    | x :: xtail -> 
        match acc with
        | [] -> group_consecutive_duplicates_aux [[x]] xtail
        | (a :: atail) :: acctail -> 
            if a = x then group_consecutive_duplicates_aux ((x :: a :: atail) :: acctail) xtail  
            else group_consecutive_duplicates_aux ([x] :: acc) xtail in
    rev (group_consecutive_duplicates_aux [] items);;
    
(* run-length encode consecutive duplicates into (count, item) tuples *)
(* TODO: Similar to above, there is probably a more elegant solution. *)
let run_length_encode_to_tuples items = 
    let rec encode_aux acc xs =
        match xs with
        | [] -> acc
        | x :: xtail ->
            match acc with
            | [] -> encode_aux [(1, x)] xtail
            | (n, k) :: acctail -> 
                if k = x then encode_aux ((n + 1, k) :: acctail) xtail
                else encode_aux ((1, x) :: acc) xtail in
    rev (encode_aux [] items);;
    
module Rle = struct
    type 'a rle =
        | One of 'a
        | Many of int * 'a;;

    let string_of_a_rle string_of_a a_rle = 
        match a_rle with
        | One a -> "One " ^ (string_of_a a) 
        | Many (n, a) -> "Many " ^ (string_of_int n) ^ " " ^ (string_of_a a);;

    (* run-length encode consecutive duplicates into singletons 
       or (count, item) tuples *)
    let encode items =
        let rec encode_aux acc xs =
        match xs with
        | [] -> acc
        | x :: xtail ->
            match acc with
            | [] -> encode_aux [(One x)] xtail
            | (One a) :: acctail ->
                if a = x then encode_aux ((Many (2, a))::acctail) xtail
                else encode_aux ((One x)::acc) xtail
            | (Many (n, a)) :: acctail ->
                if a = x then encode_aux ((Many (n+1, a))::acctail) xtail
                else encode_aux ((One x)::acc) xtail in
        rev (encode_aux [] items);;
                    
    (* decode the run-length encoding back to the original list *)               
    let decode items =
        let rec decode_aux acc xs =
            match xs with
            | [] -> acc
            | (One x) :: xtail -> decode_aux (x::acc) xtail
            | (Many (n, x)) :: xtail -> 
                if n == 2 then decode_aux (x::x::acc) xtail
                else decode_aux (x::acc) ((Many (n - 1, x))::xtail) in
        rev (decode_aux [] items);;           
end

(* duplicate every element in a list *)
let duplicate items =
    let rec duplicate_aux acc xs =
        match xs with
        | [] -> acc
        | x :: tail -> duplicate_aux (x::x::acc) tail in
    rev (duplicate_aux [] items);;

(* replicate n times every element in a list *)
let replicate n items =
    let rec replicate_aux acc xs c =
        match xs with
        | [] -> acc
        | x :: tail -> 
            if c == 0 then replicate_aux acc tail n
            else replicate_aux (x::acc) xs (c - 1) in
    rev (replicate_aux [] items n);;

(* remove every nth item in a list *)
let drop n items =
    let rec drop_aux acc xs c =
        match xs with
        | [] -> acc
        | x :: tail -> 
        if c = 1 then drop_aux acc tail n
        else drop_aux (x::acc) tail (c-1) in
    rev (drop_aux [] items n);;

(* split a list into two parts, the first of length n, and return both parts *)
let split n items = 
    let rec split_aux acc xs c =
        match xs with
        | [] -> (acc, xs)
        | x::tail -> 
            if c = 0 then (acc, xs) 
            else split_aux (x::acc) tail (c - 1) in
    let (a, b) = split_aux [] items n in
    (rev a, b);;

(* remove the first n items from a list and return the remainder *)
let rec skip n items = 
    match items with
    | [] -> []
    | item :: tail -> 
        if n = 0 then items 
        else skip (n - 1) tail;;

(* return the first n elements of a list *)
let take n items =
    let rec take_aux acc xs c =
        match xs with
        | [] -> acc
        | x::tail -> 
            if c = 0 then acc 
            else take_aux (x::acc) tail (c-1) in
    rev (take_aux [] items n);;

(* obtain a slice from the middle of a list; i and k are zero-based indices *)
let slice i k items = 
    take (k - i + 1) (skip i items);;

(* rotate a list to the left n places; n can be negative *)    
(* alternate solution *)
let rotate2 n items =
    match items with
    | [] -> []
    | _ ->
        let len = length items in
        let modulus = ((n mod len) + len ) mod len in
        if modulus = 0 then items 
        else (slice modulus (len - 1) items ) @ (slice 0 (modulus - 1) items);;

let rotate n items = 
    match items with 
    | [] -> []
    | _ ->
        let len = length items in
        let modulus = ((n mod len) + len) mod len in
        if modulus = 0 then 
            items
        else 
            let (a, b) = split modulus items in 
            b @ a;;

(* remove the element at zero-based index i *)
let remove_at n items =
    let rec remove_at_aux acc xs c =
        match xs with
        | [] -> acc
        | x :: tail -> 
            if c = 0 then remove_at_aux acc tail (c - 1)
            else remove_at_aux (x :: acc) tail (c - 1) in
    rev (remove_at_aux [] items n);;
    
(* insert an element at zero-based index i, or the end *)
let insert_at i item items =
    let rec insert_at_aux acc xs c =
    match xs with
    | [] -> if c >= 0 then item::acc else acc
    | x :: tail -> 
        if c = 0 then insert_at_aux (x :: item :: acc) tail (c - 1)
        else insert_at_aux (x :: acc) tail (c - 1) in
    rev (insert_at_aux [] items i);;

(* create a list of integers, ascending or descending, from start to finish inclusive *)
let range start finish =
    let change = if start < finish then 1 else -1 in
    let rec range_aux acc current =
        if current = finish then current :: acc
        else range_aux (current :: acc) (current + change) in
    rev (range_aux [] start);;

(* extract a set of n randomly-chosen items from a list *)
let rand_select n items =
    let rec rand_select_aux acc xs len cur =
        match xs with
        | [] -> acc
        | _ -> 
            if cur = 0 then 
                acc 
            else 
                let i = Random.int len in 
                let (Some e) = (at i) xs in
                rand_select_aux (e::acc) (remove_at i xs) (len - 1) (cur - 1) in
    rand_select_aux [] items (length items) n;;
            
(* extract n numbers at random chosen from 1 to k *)
let lotto_pick n k = 
    rand_select n (range 1 k);;

(* generate a random permutation of a list *)
let random_permutation items = 
    rand_select (length items) items;;

(* generate all k element sublists of a given list *)
let rec sublists k items =
    if k = 0 then 
        [[]]
    else 
        match items with
        | [] -> []
        | item :: tail -> 
            let add_item_to_each lists = List.map (fun x -> item::x) lists in
            (add_item_to_each (sublists (k - 1) tail)) @ (sublists k tail);;

(* TODO: enumerate all ways of selecting disjoint groups of given size from a list *)

let compare_ints x y = 
    if x = y then 
        0 
    else if x < y then 
        -1 
    else 
        1;;

(* sort a list of lists according to the length of each sublist *)
let sort_length items = 
    List.sort (fun x y -> compare_ints (length x) (length y)) items;;

(* TODO: sort a list of lists according to the frequency of each length *)



