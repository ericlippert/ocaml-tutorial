(* Binary tree exercises from the OCaml tutorial *)

(* A completely balanced binary tree is either empty,
   or has the same number (within one) of nodes on either side,
   and both sides must also be completely balanced trees. *)
   
(* Enumerate all the completely balanced trees with n nodes. *)

type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree;;

let rec string_of_binary_tree string_of_a tree =
    match tree with
    | Empty -> "()"
    | Node (a, left, right) -> 
        "(" ^ (string_of_binary_tree string_of_a left) ^ (string_of_a a) ^ (string_of_binary_tree string_of_a right) ^ ")";;
    
(* select many takes a list of x and a function that maps
   each x into a list of y.  It then concatenates all those
   lists together. The select many operation is the bind 
   operation on the list monad. *)
   
let select_many xs f =
    List.concat (List.map f xs);;

(* cross join takes two lists, xs and ys, and a function that takes an
  x and a y. It produces the list that is all combinations of x and y
  run through the function. *)

let cross_join xs ys f =
    select_many xs (fun x -> List.map (fun y -> (f x y)) ys);;
    
let cartesian_product xs ys =
    cross_join xs ys (fun x y -> (x, y));;
    
let rec cbal n =
    let join x y = Node (0, x, y) in
    if n = 0 then 
        [ Empty ]
    else if n mod 2 = 0 then 
        let some = cbal ((n - 1) / 2) in
        let others = cbal (n - 1 - (n - 1) / 2) in
        (cross_join some others join) @ (cross_join others some join)
    else 
        let some = cbal ((n - 1) / 2) in
        cross_join some some join;;
        
(* Given two binary trees, is one a mirror image of the other? *)
        
let rec is_mirror_image x y =
    match (x, y) with
    | (Empty, Empty) -> true
    | (Node (_, xleft, xright), Node (_, yleft, yright)) -> 
        (is_mirror_image xleft yright) && (is_mirror_image xright yleft)
    | _ -> false;;
        
let is_symmetrical x =
    is_mirror_image x x;;
    
let rec construct_search_tree values =
    let rec insert tree value = 
        match tree with
        | Empty -> Node (value, Empty, Empty)
        | Node (oldvalue, oldleft, oldright) -> 
            if value < oldvalue then Node(oldvalue, (insert oldleft value), oldright)
            else if value > oldvalue then Node(oldvalue, oldleft, (insert oldright value))
            else tree in
    List.fold_left insert Empty values;;
            
           
 (* A height-balanced tree is like a complete tree, except that the property
    held throughout is that the longest path to a leaf on both sides differs
    by no more than one. *)
    
(* Enumerate all the height-balanced trees of a given height *)
let rec all_height_balanced_trees h =
    let join x y = Node (0, x, y) in
    if h = 0 then 
        [ Empty ]
    else if h = 1 then
        [ Node (0, Empty, Empty) ]
    else  
        let one_less = all_height_balanced_trees (h - 1) in
        let two_less = all_height_balanced_trees (h - 2) in
        (cross_join one_less two_less join) @ (cross_join two_less one_less join) @ (cross_join one_less one_less join);;
        
