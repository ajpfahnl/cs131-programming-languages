(* CS 131 Homework 1 - hw1test.ml *)

let subset_test0 = subset [] [1;2;3]
let subset_test1 = subset [3;1;3] [1;2;3]
let subset_test2 = not (subset [1;3;7] [4;1;3])
let subset_test3 = not (subset [1] [])
let subset_test4 = not (subset [1] [0])

let equal_sets_test0 = equal_sets [1;3] [3;1;3]
let equal_sets_test1 = not (equal_sets [1;3;4] [3;1;3])
let equal_sets_test2 = equal_sets [[1;2];[3;4]] [[3;4];[1;2]];; 
let equal_sets_test3 = not (equal_sets [[1;2];[3;4]] [[2;1];[4;3]])
let equal_sets_test4 = equal_sets [1;2] [2;1]

let set_union_test0 = equal_sets (set_union [] [1;2;3]) [1;2;3]
let set_union_test1 = equal_sets (set_union [3;1;3] [1;2;3]) [1;2;3]
let set_union_test2 = equal_sets (set_union [] []) []
let set_union_test3 = equal_sets (set_union [1] []) [1]
let set_union_test4 = equal_sets (set_union [] [1]) [1]
let set_union_test5 = equal_sets (set_union [1] [1]) [1]

let set_all_union_test0 =
  equal_sets (set_all_union []) []
let set_all_union_test1 =
  equal_sets (set_all_union [[3;1;3]; [4]; [1;2;3]]) [1;2;3;4]
let set_all_union_test2 =
  equal_sets (set_all_union [[5;2]; []; [5;2]; [3;5;7]]) [2;3;5;7]
let set_all_union_test3 =
  equal_sets (set_all_union [[]; []]) []
let set_all_union_test4 =
  equal_sets (set_all_union [[1;2;3]]) [1;2;3]
let set_all_union_test5 =
  equal_sets (set_all_union [[1;2]; [1;4]]) [1;2;4]
let set_all_union_test6 =
  equal_sets (set_all_union [[1;4]; [1;4]]) [1;4]
let set_all_union_test6 =
  equal_sets (set_all_union [[1;2]; [2;1]; [3;4]]) [1;2;3;4]
let set_all_union_test7 =
  equal_sets (set_all_union [[]]) []
  
let computed_fixed_point_test0 =
  computed_fixed_point (=) (fun x -> x / 2) 1000000000 = 0
let computed_fixed_point_test1 =
  computed_fixed_point (=) (fun x -> x *. 2.) 1. = infinity
let computed_fixed_point_test2 =
  computed_fixed_point (=) sqrt 10. = 1.
let computed_fixed_point_test3 =
  ((computed_fixed_point (fun x y -> abs_float (x -. y) < 1.)
			 (fun x -> x /. 2.)
			 10.)
   = 1.25)
let computed_fixed_point_test4 = 
  computed_fixed_point (=) (fun x -> x) 100 = 100


(* An example grammar for a small subset of Awk.  *)

type awksub_nonterminals =
  | Expr | Lvalue | Incrop | Binop | Num

let awksub_rules =
   [Expr, [T"("; N Expr; T")"];
    Expr, [N Num];
    Expr, [N Expr; N Binop; N Expr];
    Expr, [N Lvalue];
    Expr, [N Incrop; N Lvalue];
    Expr, [N Lvalue; N Incrop];
    Lvalue, [T"$"; N Expr];
    Incrop, [T"++"];
    Incrop, [T"--"];
    Binop, [T"+"];
    Binop, [T"-"];
    Num, [T"0"];
    Num, [T"1"];
    Num, [T"2"];
    Num, [T"3"];
    Num, [T"4"];
    Num, [T"5"];
    Num, [T"6"];
    Num, [T"7"];
    Num, [T"8"];
    Num, [T"9"]]

let awksub_grammar = Expr, awksub_rules

let awksub_test0 =
  filter_reachable awksub_grammar = awksub_grammar

let awksub_test1 =
  filter_reachable (Expr, List.tl awksub_rules) = (Expr, List.tl awksub_rules)

let awksub_test2 =
  filter_reachable (Lvalue, awksub_rules) = (Lvalue, awksub_rules)

let awksub_test3 =
  filter_reachable (Expr, List.tl (List.tl awksub_rules)) =
    (Expr,
     [Expr, [N Expr; N Binop; N Expr];
      Expr, [N Lvalue];
      Expr, [N Incrop; N Lvalue];
      Expr, [N Lvalue; N Incrop];
      Lvalue, [T "$"; N Expr];
      Incrop, [T "++"];
      Incrop, [T "--"];
      Binop, [T "+"];
      Binop, [T "-"]])

let awksub_test4 =
  filter_reachable (Expr, List.tl (List.tl (List.tl awksub_rules))) =
    (Expr,
     [Expr, [N Lvalue];
      Expr, [N Incrop; N Lvalue];
      Expr, [N Lvalue; N Incrop];
      Lvalue, [T "$"; N Expr];
      Incrop, [T "++"];
      Incrop, [T "--"]])

type giant_nonterminals =
  | Conversation | Sentence | Grunt | Snore | Shout | Quiet

let giant_grammar =
  Conversation,
  [Snore, [T"ZZZ"];
   Quiet, [];
   Grunt, [T"khrgh"];
   Shout, [T"aooogah!"];
   Sentence, [N Quiet];
   Sentence, [N Grunt];
   Sentence, [N Shout];
   Conversation, [N Snore];
   Conversation, [N Sentence; T","; N Conversation]]

let giant_test0 =
  filter_reachable giant_grammar = giant_grammar

let giant_test1 =
  filter_reachable (Sentence, List.tl (snd giant_grammar)) =
    (Sentence,
     [Quiet, []; Grunt, [T "khrgh"]; Shout, [T "aooogah!"];
      Sentence, [N Quiet]; Sentence, [N Grunt]; Sentence, [N Shout]])

let giant_test2 =
  filter_reachable (Quiet, snd giant_grammar) = (Quiet, [Quiet, []])

let giant_test3 = 
  filter_reachable (Conversation, []) = (Conversation, [])

let giant_test4 = 
  filter_reachable (Conversation, [Shout, [N Snore]]) = (Conversation, [])

let giant_test5 = 
  filter_reachable (Conversation, [Conversation, [N Snore]]) = (Conversation, [Conversation, [N Snore]])

let giant_test6 = 
  filter_reachable (Conversation, 
    [Conversation, [N Snore]; Quiet, [T "argh"]; Snore, [T "blah"]]) = (Conversation, [Conversation, [N Snore]; Snore, [T "blah"]])