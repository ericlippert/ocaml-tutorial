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
        
(* What is the smallest number of nodes that can be in a height-balanced tree of given height? *)
(* TODO: This is basically an unmemoized minor variation on computing Fibonacci numbers, and
   hence has exponential performance. *)
let rec min_nodes h =
    if h = 0 then 0
    else if h = 1 then 1
    else 1 + min_nodes (h - 2) + min_nodes(h - 1);;

(* What is the height of the highest height-balanced tree with n nodes? *)
let rec max_height n = 
    if n = 0 then 0
    else 
        (* Let's make the recursive assumption that we've solved the smaller 
           problem: what is the maximum height of a tree with one fewer nodes? *)
        let h = max_height (n - 1) in
        
        (* The highest tree with (n - 1) nodes of height h clearly has 
            a subtree of height (h - 1) and another subtree of height 
            (h - 1) or (h - 2).  WOLOG, let's assume that the right subtree
            is at least (h - 1) high.  Our strategy here is to find the 
            tree with (n - 1) nodes where the right tree is of height
            (h - 1) and the right tree has the least possible number of 
            nodes in it.  This means that the left tree will have the
            most possible nodes in it. How many nodes are in the right tree? *)
        
        let min_subtree = min_nodes (h - 1) in
        
        (* Therefore the most nodes that can possibly be in the left subtree is *)
        
        let max_subtree = (n - 1) - 1 - min_subtree in
        
        (* What is the maximum possible height of the left subtree? Plainly it
           cannot be h, because then we would be able to construct a left tree
           of size h, a right tree of size (h - 1), and a total tree of size 
           (h + 1), but the assumption was that we have found the highest possible
           tree with (n - 1) nodes. So we only have enough nodes on the left side
           to construct a tree with height (h - 1), tops. *)
       
        (* Suppose we add a node to the left hand side, making the total
           number of nodes in the tree n.  We keep the right side the
           same lightest possible subtree of height (h - 1). Can we 
           reorganize the (max_subtree + 1) nodes on the left into a tree
           of height h? 
           
           If yes then we can construct a height-balanced tree of height
           h on the left and (h-1) on the right, which is a tree of total 
           height (h + 1).  
           
           If no then even with the lightest possible tree on the right side
           we still can only get the left side to have height (h - 1) when
           we add a node to it. 
           
           Again we make the recursive assumption that we can solve the smaller
           problem. *)
           
        let new_h = max_height (max_subtree + 1) in
        if new_h = h then h + 1
        else h;;
        
        (* Let's consider an example. How high is the highest tree with 7 nodes?
           We assume that we have a solution for the highest tree with 0 through 6
           nodes. The highest possible tree with 6 nodes is 3 high. That means that
           at least one side is 2 high (in fact they both are.)  Suppose the right
           side is 2 high and as light as possible; the lightest possible tree of
           height 2 has 2 nodes in it.  That leaves 3 nodes for the left side. Suppose
           we added a 4th node to the left side; could we make a tree of height 3?
           Yes! Therefore the maximum height of a tree with 7 nodes has height 3 on
           one side and height 2 on the other. 
           
           Now how high can we make a tree with 8 nodes? The highest tree with 7
           nodes has height 3 on the high side. What's the least number of nodes
           in that tree? We can make a 4-node tree of height 3. That means that 
           in the highest tree with 7 nodes, we have at most 2 nodes on the 
           light side. Can we add a third and rearrange that into a tree of
           height 3? No. So the highest we can make a tree with 8 nodes is the 
           same as the highest we can make a tree with 7 nodes: 3 high. *)
        
        
            
            
            
