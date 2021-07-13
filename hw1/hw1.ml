(* CS 131 Homework 1 - hw1.ml *)

(* Q1: subset - finds each element of a in b recursively *)
let rec subset a b = match a with
    | [] -> true
    | _ -> if List.exists (fun bi -> bi = List.hd a) b
           then subset (List.tl a) b 
           else false;;

(* Q2: equal sets - checks if a and b are subsets of each other *)
let equal_sets a b = 
    if subset a b && subset b a
    then true
    else false;;

(* Q3: set union - recursively adds elements of a not in b to b *)
let rec set_union a b = match a with
    | [] -> b
    | _ -> if List.exists (fun bi -> bi = List.hd a) b
           then set_union (List.tl a) b
           else set_union (List.tl a) (List.hd a::b);;

(* Q4: set all union *)
let rec set_all_union a = match a with
    | [] -> []
    | h::[] -> h
    | h1::h2::t -> set_all_union ((set_union h1 h2)::t);;

(* Q5: Russell's Paradox - whether a set is a member of itself *)
(* let self_member s *)
(* 
It's impossible to write function that can determine if a set is a member
    of itself. Russell's Paradox is a clear example of why this is. If the
    input set to self_member is "the set of all sets that are not members of 
    themselves", then we might return false by the definition of the set, but
    we might also return true since the set can be considered a member of 
    itself since it is not a member of itself. This is nondeterministic, so
    we can't write a function, since it needs to be deterministic.

    Ref: https://plato.stanford.edu/entries/russell-paradox/
*)

(* Q6: returns the computed fixed point for f with respect to x, assuming that
       eq is the equality predicate for f's domain *)
let rec computed_fixed_point eq f x =
    if eq (f x) x
    then x
    else computed_fixed_point eq f (f x);;

(* Q7: returns a copy of the grammar g with all unreachable rules removed 
       should preserve the order of rules *)
type ('nonterminal, 'terminal) symbol =
    | N of 'nonterminal
    | T of 'terminal

let rec subset_n rhs = match rhs with
    | [] -> []
    | N hd :: tl -> hd :: subset_n tl
    | T hd :: tl -> subset_n tl;;

let rec find_reachable n rules = match rules with
    | [] -> []
    | (lhs, rhs) :: tl -> if lhs = n
        then set_union (subset_n rhs) (n :: find_reachable n tl)
        else (n :: find_reachable n tl);;

let rec construct nset rules = match nset with 
    | [] -> []
    | hd :: tl -> set_union (hd :: find_reachable hd rules) (construct tl rules);;

let filter_reachable g = 
    let reachable_ns = computed_fixed_point equal_sets (fun x -> (construct x (snd g))) [fst g] in
    fst g, List.filter (fun (n, rhs) -> List.mem n reachable_ns) (snd g);;