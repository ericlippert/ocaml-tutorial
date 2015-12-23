(* arithmetic exercises from OCAML tutorial *)

open List_exercises;;

(* a predicate to test primality *)
let is_prime x = 
    if x < 2 then 
        false
    else 
        let rec is_prime_aux d =
            if x = d then true
            else if (x mod d) = 0 then false
            else is_prime_aux (d + 1) in
        is_prime_aux 2;;
        
(* Euclid's algorithm *)
let rec gcd a b = 
    if b = 0 then a 
    else gcd b (a mod b);;

(* two numbers are coprime if their gcd is one *)
let coprime a b = 
    1 = gcd a b;;

(* Euler's totient function is the number of integers less than m that are coprime to m *)
let phi m = 
    if m = 1 then 
        1
    else 
        let rec phi_aux count current =
            if current >= m then count
            else if (coprime current m) then phi_aux (count + 1) (current + 1)
            else phi_aux count (current + 1) in
    phi_aux 0 1;;
    
(* is p a prime factor of n? It must be both prime and a factor *)
let is_prime_factor p n = 
    if (is_prime p) then (n mod p = 0) 
    else false;;

(* produce the prime factorization of a number as a flat list *)
let factors n =
    let rec factors_aux acc current remaining =
        if current = remaining then current::acc
        else if (is_prime_factor current remaining) then factors_aux (current::acc) current (remaining / current)
        else factors_aux acc (current + 1) remaining in
    if n <= 1 then [1]
    else List.rev (factors_aux [] 2 n);;
    
(* produce the prime factorization of a number as tuples counting each factor *)
let factorization n = 
    run_length_encode_to_tuples (factors n);;
       
(* The integer power function computes x to the y. 
   
   Note that ** only does floats in ocaml. 
   TODO: Not a very efficient algorithm; repeated squaring would be better. *)
let power x y =
    if x = 0 then (
        if y = 0 then failwith "zero to the zero is undefined"
        else 0)
    else 
        let rec power_aux acc current =
            if current = 0 then acc
            else power_aux (acc * x) (current - 1) in
        power_aux 1 y;;
        
(* A faster version of the totient function *)
let phi_improved m =
    if m = 1 then   
        1
    else
        List.fold_left 
            (fun product (count, factor) -> product * (factor - 1) * (power factor (count - 1)))
            1 (factorization m);;
        
       
(* All the primes between a and b, inclusive. *)
let all_primes a b =
    List.filter is_prime (range a b);;

(* the two primes that add to even number n *)
let goldbach n =
    let x = first_or_default (fun x -> (is_prime (n - x))) (all_primes 2 n) 0 in
    (x, n - x);;
    
