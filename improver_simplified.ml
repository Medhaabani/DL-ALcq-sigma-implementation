(* Improved ALCQ prover.
   Constructors currently not treated: AllC, SomeC (not generated by parser)

   THIS VERSION: Introduced Big_int, but in an incomplete manner. All explicit or
   implicit comparisons of Big_int or constructors containing them have to be
   done with specific comparison functions, not with = or < etc.  

*)

open Verif_condition.VerifCondition ;;
open Lang ;;
open Auxil;;

(*
type ('nr, 'nc, 'ni) rule_rep =
  Clash_rep
| AndRule_rep of ('nr, 'nc, 'ni) form
| OrRule_rep of ('nr, 'nc, 'ni) form
| ConjRule_rep of ('nr, 'nc, 'ni) form
| DisjRule_rep of ('nr, 'nc, 'ni) form
| Todo_rep

type ('a, 'b) proof_result = 
  TablUnsatisf of 'a
| TablSatisf of 'b

type ('nr, 'nc, 'ni) trace = 
  Trace of ('nr, 'nc, 'ni) rule_rep * ('nr, 'nc, 'ni) trace list

type ('nr, 'nc, 'ni) trace_constr = 
  CTrace of ('nr, 'nc, 'ni) trace       (* complete trace *)
| ITrace of ('nr, 'nc, 'ni) trace list * int * ('nr, 'nc, 'ni) rule_rep (* incomplete trace *)

*)


    (* ----------------------------------------------------------------------  *)
    (* Search *)



(* TODO: reconsider termination problem described in BS p.11
--> problem does not seem to occur here, since exist-rules are applied only once *)

(* Proof search without proof trace construction
let apply_rules eqs = function
  | Branch([], ([], ([], ([], _)))) -> 
    failwith "apply_rules should have active branch"
  | Branch([], ([], ([], (apf::apfs, ia)))) -> 
    List.map (classify_new eqs eqs eqs)
      (apply_permanent eqs eqs eqs (Branch([], ([], ([], (apfs, ia))))) apf)
  | Branch([], ([], (asf::asfs, (ap, ia)))) -> 
    List.map (classify_new eqs eqs eqs)
      (apply_splitting eqs eqs eqs (Branch([], ([], (asfs, (ap, ia))))) asf)
  | Branch([], (alf::alfs, (asf, (ap, ia)))) -> 
    List.map (classify_new eqs eqs eqs)
        (apply_linear eqs eqs eqs (Branch([], (alfs, (asf, (ap, ia))))) alf)
  | br -> [classify_new eqs eqs eqs br]

let rec search eqs = function
  | [] ->  []
  | br :: brs -> 
    if contains_clash_branch eqs eqs eqs br
    then search eqs brs
    else if is_inactive br 
    then [br]
    else search eqs ((apply_rules eqs br) @ brs)

let prove fs = 
  let eqs = {equal = (=)} in 
  search eqs [Branch(fs, ([], ([], ([], Inactive_form([], ([], ([], ([], []))))))))]
*)

(* Proof search with proof trace construction *)
(*
let map_classify_branches eqs = function
  | AppRes(n, rr_opt, brs) -> AppRes(n, rr_opt, List.map (classify_new eqs eqs eqs) brs)

let apply_rules_pt eqs = function
  | Branch([], ([], ([], ([], _)))) -> 
    failwith "apply_rules_pt should have active branch"
  | Branch([], ([], ([], (apf::apfs, ia)))) -> 
    map_classify_branches eqs (apply_permanent eqs eqs eqs (Branch([], ([], ([], (apfs, ia))))) apf)
  | Branch([], ([], (asf::asfs, (ap, ia)))) -> 
    map_classify_branches eqs (apply_splitting eqs eqs eqs (Branch([], ([], (asfs, (ap, ia))))) asf)
  | Branch([], (alf::alfs, (asf, (ap, ia)))) -> 
    map_classify_branches eqs (apply_linear eqs eqs eqs (Branch([], (alfs, (asf, (ap, ia))))) alf)
  | br -> AppRes(Nat Big_int.zero_big_int, None, [classify_new eqs eqs eqs br])
*)

(* collects complete failed subproofs and constructs a proof trace out of them  *)
let rec failed_subproof current = function
  | [] -> [CTrace current]
  | ITrace(subtrs, n, rr) :: traces -> 
    if Big_int.eq_big_int 
      (Big_int.add_big_int (Big_int.big_int_of_int (List.length subtrs)) (Big_int.big_int_of_int 1)) 
      (integer_of_nat n)
    then failed_subproof (Trace(rr, List.rev (current::subtrs))) traces
    else ITrace(current::subtrs, n, rr)::traces
  | CTrace tr :: traces -> failwith "failed_subproof: CTrace"

let rec search_pt eqs ord = function
  | ([CTrace tr], []) ->  TablUnsatisf tr
  | (traces, br :: brs) -> 
    if contains_clash_branch eqs eqs eqs br
    then search_pt eqs ord (failed_subproof (Trace(Clash_rep, [])) traces, brs)
    else if is_inactive eqs eqs (eqs, ord) br 
    then TablSatisf br
    else 
      (let nvl = {new_var_list = fresh_string_literal} in 
      (match apply_rules_pt eqs eqs (eqs, nvl, ord) br with
      | AppRes(_, None, new_brs) -> search_pt eqs ord (traces, new_brs @ brs)
      | AppRes(n, Some rr, new_brs) -> search_pt eqs ord (ITrace([], n, rr)::traces, new_brs @ brs)))
  | (_, []) -> failwith "search_pt: wrongly constructed trace"


let prove_pt fs = 
  let eqs = {equal = (=)} in 
  let ord = {less_eq = (<=); less = (<)} in
  search_pt eqs ord ([], [Branch(fs, ([], ([], ([], Inactive_form([], ([], ([], ([], []))))))))])

(* Tests *)


(* for prove_pt:
# prove_pt [FactFm (Inst("x", BinopC(Conj, AtomC(true, "a"), AtomC(true, "b"))))] ;;
# prove_pt [FactFm (Inst("x", BinopC(Conj, AtomC(true, "a"), AtomC(false, "a"))))] ;;
# prove_pt [FactFm (Inst("x", 
   BinopC(Disj, 
   BinopC(Conj, AtomC(true, "a"), AtomC(false, "a")),
   BinopC(Conj, AtomC(true, "b"), AtomC(false, "b")))))] ;;

 *)

(*
for contains_numrestr_clash:

let br1 = 
  Branch ([],
   [FactFm (Inst ("x", NumRestrC (Lt, Nat (Big_int.big_int_of_int 3), "r", AtomC (true, "c"))))],
   [], [],
   Inactive_form ([],
    [FactFm (Inst ("y1", AtomC (true, "c")));
     FactFm (Inst ("y2", AtomC (true, "c")));
     FactFm (Inst ("y3", AtomC (true, "c")))],
    [FactFm (AtomR (true, "r", "x", "y1"));
     FactFm (AtomR (true, "r", "x", "y2"));
     FactFm (AtomR (true, "r", "x", "y3"))],
    [FactFm (Eq (false, "y1", "y2")); FactFm (Eq (false, "y3", "y2"));
     FactFm (Eq (false, "y3", "y1"))],
    []))
# contains_numrestr_clash br1 ;;

prove_pt [FactFm (Inst ("e", NumRestrC (Lt, Nat (Big_int.big_int_of_int 1), "r", SubstC(AtomC (true, "A"),RSubst("r", RDiff, ("e", "n")))))); FactFm (Inst ("e1", AtomC (true, "A"))); FactFm (AtomR(true, "r", "e", "e1"))] ;;
prove_pt [FactFm (Inst("x", 
   BinopC(Disj, 
   BinopC(Conj, AtomC(true, "a"), AtomC(false, "a")),
   BinopC(Conj, AtomC(true, "b"), AtomC(false, "b")))))] ;;

 *)
