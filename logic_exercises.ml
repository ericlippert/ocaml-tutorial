(* logic exercises from OCAML tutorial *)

(* a sequence of the n-bit Gray codes *)
let rec gray n =
    if n = 0 then [""]
    else let smaller = gray (n - 1) in
    let first = List.map (fun x -> "0" ^ x) smaller in
    let last = List.rev (List.map (fun x -> "1" ^ x) smaller) in
    first @ last;;
    
   
