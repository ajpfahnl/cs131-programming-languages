type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

(* Question 1 *)

let convert_grammar gram1 = 
  let rec find_rhss g n = match g with
    | [] -> []
    | (lhs, rhs) :: tl -> 
      if lhs = n
      then rhs :: (find_rhss tl n)
      else find_rhss tl n in
  fst gram1, find_rhss (snd gram1)

(* Question 2 *)

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

let parse_tree_leaves tree = 
  let rec parse_tree_leaves_helper list = match list with
    | [] -> []
    | hd :: tl -> match hd with
      | Node (n, tree) -> parse_tree_leaves_helper tree @ parse_tree_leaves_helper tl
      | Leaf term -> term :: parse_tree_leaves_helper tl in
  parse_tree_leaves_helper [tree]

(* Question 3 *)
(* 
  make_matcher gram: returns matcher for grammar gram
   * apply to acceptor accept and fragment frag
   -> must try grammar rules in order
   -> return result of calling accept on suffix corresponding to
      first acceptable matching prefix of frag
   * acceptable if accept succeeds when given the suffix fragment
     immediately following matching prefix
   -> no acceptable match, return None
   -> else return what accept returns
*)

(* VERSION 1: not compatible with make_parser *)
(*
let rec make_matcher_helper production nalts accept frag = 
  let rec checker nalts accept frag = match nalts with
    | [] -> accept frag
    | nalts_hd :: nalts_tl -> (match frag with
      | [] -> None
      | frag_hd :: frag_tl -> (match nalts_hd with
        | T s -> 
          if s = frag_hd
          then checker nalts_tl accept frag_tl
          else None
        | N s -> (make_matcher_helper production s (production s))
                 (fun f -> (checker nalts_tl) accept f)
                 frag
      )
    )
  in
  match nalts with
    | [] -> None
    | nalts_hd :: nalts_tl -> (match (checker nalts_hd accept frag) with
      | None -> make_matcher_helper production nalts_tl accept frag
      | x -> x
    )

let make_matcher gram =
  let nterm = fst gram
  and production = snd gram in
  let start_nalts = production nterm in
  fun accept frag -> make_matcher_helper production start_nalts accept frag
*)

(* VERSION 2: refactored for use with make_parser (Q4) *)

let rec make_matcher_helper production nterm nalts accept frag enced = 
  let rec checker nalts accept frag enced = match nalts with
    | [] -> accept frag enced
    | nalts_hd :: nalts_tl -> (match frag with
      | [] -> None
      | frag_hd :: frag_tl -> (match nalts_hd with
        | T s -> 
          if s = frag_hd
          then checker nalts_tl accept frag_tl enced
          else None
        | N s -> (make_matcher_helper production s (production s))
                 (fun f -> (checker nalts_tl) accept f)
                 frag
                 enced
      )
    )
  in
  match nalts with
    | [] -> None
    | nalts_hd :: nalts_tl -> (match (checker nalts_hd accept frag ((nterm, nalts_hd)::enced)) with
      | None -> make_matcher_helper production nterm nalts_tl accept frag enced
      | x -> x
    )

let make_matcher gram =
  let nterm = fst gram
  and production = snd gram in
  let start_nalts = production nterm in
  fun accept frag -> make_matcher_helper production nterm start_nalts (fun f e -> accept f) frag []

(* Question 4 *)
(* 
  make_parser gram: returns a parser for the grammar gram 
   * apply to fragment frag, parser returns optional parse tree
   -> frag cannot be parsed fully: return None
   -> else: return Some tree, corresponding parse tree
   * grammar rules in same order as make_matcher
*)

let rec next_level slist enced =
  let same_level s = match s with
    | N s -> (match enced with
      | [] -> ([], Node (s, []))
      | enced_hd :: enced_tl ->
        let (enced_r, tree) = next_level (snd enced_hd) enced_tl in
        enced_r, Node (s, tree))
    | T s -> (enced, Leaf s)
  in
  match slist with
    | [] -> enced, []
    | slist_hd :: slist_tl ->
      let (enced_r, tree) = same_level slist_hd in
      let (enced_r_new, tree_new) = next_level slist_tl enced_r in
      (enced_r_new, tree :: tree_new)

let make_parser gram = fun frag -> 
  let accept_parse frag enced = match frag with
    (* empty acceptor *)
    | _::_ -> None
    | x -> Some (List.rev enced)
  and nterm = fst gram
  and production = snd gram in
  let start_nalts = production nterm in
  let s_enced = make_matcher_helper production nterm start_nalts accept_parse frag [] in
  match s_enced with
    | None -> None
    | Some [] -> None
    | Some x -> (match x with
      | [] -> None
      | hd :: _ -> 
      let slist = [N (fst (hd))] in
      let ptup = next_level slist x in
      (match (snd ptup) with
        | [] -> None
        | hd :: _ -> Some hd
      )
    )