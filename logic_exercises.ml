(* logic exercises from OCaml tutorial *)

(* a sequence of the n-bit Gray codes *)
let rec gray n =
    if n = 0 then [""]
    else let smaller = gray (n - 1) in
    let first = List.map (fun x -> "0" ^ x) smaller in
    let last = List.rev (List.map (fun x -> "1" ^ x) smaller) in
    first @ last;;

(* Huffman encoding *)

type huffman_tree = 
    | Leaf of string
    | Interior of huffman_tree * huffman_tree;;

let huffman frequencies = 
 
    (* Construct a priority queue of leaves from the frequencies *)
    let queue = 
        List.fold_left 
            (fun q (item, frequency) -> 
                Priority_queue.insert q frequency (Leaf item)) 
            Priority_queue.empty 
            frequencies in
            
    (* Recursively combine the lowest two elements in the 
    priority tree into a single element until only one element
    remains; this is the Huffman tree. *)
    
    let rec queue_to_tree q = 
        if q = Priority_queue.empty then
            failwith "huffman encoding requires input"
        else 
            let (first_priority, first_tree, dequeued_once) = Priority_queue.extract q in
            if dequeued_once = Priority_queue.empty then
                first_tree
            else
                let (second_priority, second_tree, dequeued_twice) = Priority_queue.extract dequeued_once in
                queue_to_tree (Priority_queue.insert dequeued_twice (first_priority + second_priority) (Interior (first_tree, second_tree))) in
    let tree = queue_to_tree queue in

    (* Turn the Huffman tree into a table showing the codes *)

    let rec tree_to_table prefix tr =
        match tr with
        | Leaf value -> [(value, prefix)]
        | Interior (left, right) -> (tree_to_table (prefix ^ "0") left) @ (tree_to_table (prefix ^ "1") right) in
    tree_to_table "" tree;;  
 
