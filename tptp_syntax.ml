(* Generation of TPTP syntax with provers such as EProver and Vampire.
*)

open Big_int ;;
open Verif_condition.VerifCondition ;;
open Lang ;;


(* Big_int does not use the traditional equality, but a special equality eq_big_int.
   Therefore, an extra argument has been added to some functions (insert, union ...).
   Quite an ugly hack.
 *)

let insert eq x xs = if (List.exists (eq x) xs) then xs else x :: xs 

let rec union eq xs ys = 
  match xs with
  | [] -> ys
  | x :: xs' -> insert eq x (union eq xs' ys)


let triple_union eq (r1, c1, i1) (r2, c2, i2) = 
  (union eq r1 r2, union eq c1 c2, union eq i1 i2)

let remove_bound vars = List.filter (function Bound _ -> false | _ -> true) vars

let triple_remove_bound (r, c, i) = (remove_bound r, remove_bound c, remove_bound i)


type extended_binop = EBConj | EBDisj | EBImpl | EBIff

let binop_into_extended_binop = function
  | Conj -> EBConj
  | Disj -> EBDisj
  | Impl -> EBImpl

type ('nr, 'nc, 'ni) fof_form = 
    FFalse | FTrue
  | FPred of 'nc * 'ni
  | FRel of bool * 'nr * 'ni * 'ni
  | FEq of bool * 'ni * 'ni
  | FNeg of (('nr, 'nc, 'ni) fof_form)
  | FBinop of extended_binop * (('nr, 'nc, 'ni) fof_form) * (('nr, 'nc, 'ni) fof_form)
  | FQuantif of quantif * string * (('nr, 'nc, 'ni) fof_form)                  (* implicit quantifs with deBruijn indices *)
  | FQuantifExpl of quantif * ('ni list) * (('nr, 'nc, 'ni) fof_form) (* explicit quantifs, with names *)
  | FLetC of (('nr, 'nc, 'ni) fof_form) * (('nr, 'nc, 'ni) fof_form) 
  | FLetR of (('nr, 'nc, 'ni) fof_form) * (('nr, 'nc, 'ni) fof_form) 

(* in the FLets, the first three arguments could be a let_def, 
   but this would make the type mutually recursive. *)

(* TODO: still used?
type ('nr, 'nc, 'ni) let_def = 
  LetDef of 'nr * ('ni * 'ni) * (('nr, 'nc, 'ni) fof_form)
*)

let rec conjs = function
  | [] -> FTrue
  | [x] -> x
  | x :: xs -> FBinop (EBConj, x, conjs xs)

let rec gen_binders n =
  if sign_big_int n = -1
  then []
  else Bound n :: gen_binders (pred_big_int n)

let rec pairs = function
  | [] -> []
  | x :: xs -> (List.map (fun y -> (x, y)) xs) @ pairs xs


(* ------------------------------------------------------------ *)

let lift_var_multiple n = function 
  | Free v -> Free v
  | Bound k -> Bound (add_big_int k n)

let replace x vmap = 
  try List.assoc x vmap with
    Not_found -> x

let propagate_rmap rmap = function
  | RSubst (r, rop, (y1, y2)) -> RSubst (replace r rmap, rop, (y1, y2))
  | isubst -> isubst

let lift_vmap vmap = List.map (fun (x, y) -> (lift_var zero_big_int x, lift_var zero_big_int y)) vmap



(* The following functions all work with deBruijn indices, 
   also for generated relations and concepts   *)
let transl_fof_numrestrc x n r c = 
  let x_lifted = lift_var_multiple n x in
  let vars = gen_binders (pred_big_int n) in
  let rs = List.map (fun y -> FRel(true, r, x_lifted, y)) vars in
  let cs = List.map (fun y -> FPred(c, y)) vars in
  let ineqs = List.map (fun (x, y) -> FEq (false, x, y)) (pairs vars) in
  FQuantifExpl (QEx, vars, conjs (rs @ cs @ ineqs))


let rec transl_fof_concept rmap x = function 
  | AtomC(cn) -> FPred(cn, x)
  | Top -> FTrue
  | Bottom -> FFalse
  | NotC(c) -> FNeg(transl_fof_concept rmap x c)
  | AndC(c1, c2) -> FBinop(EBConj, transl_fof_concept rmap x c1, transl_fof_concept rmap x c2)
  | OrC(c1, c2) -> FBinop(EBDisj, transl_fof_concept rmap x c1, transl_fof_concept rmap x c2)
  | NumRestrC (Le, n, r, c) -> FNeg (transl_fof_concept rmap x (NumRestrC (Geq, n, r, c)))
  | NumRestrC (Geq, n, r, (AtomC cn)) -> 
    if eq_big_int n zero_big_int 
    then FTrue 
    else transl_fof_numrestrc x n (replace r rmap) cn
  | NumRestrC (Geq, n, r, c) -> 
    if eq_big_int n zero_big_int 
    then FTrue 
    else  FLetC (transl_fof_concept rmap (Bound zero_big_int) c,
           transl_fof_numrestrc x n (replace r rmap) (Bound zero_big_int))
  | SubstC _ -> failwith "transl_fof_concept: no substitutions in concepts"



let transl_fof_subst = function
  | RSubst(r, RDiff, (y1, y2)) -> 
    FBinop (EBConj, 
            FRel (true, r, Bound zero_big_int, Bound unit_big_int),
            FNeg (FBinop (EBConj, 
                          FEq (true, Bound zero_big_int, lift_var_multiple (big_int_of_int 2) y1), 
                          FEq (true, Bound unit_big_int, lift_var_multiple (big_int_of_int 2) y2))))
  | RSubst(r, RAdd, (y1, y2)) -> 
    FBinop (EBDisj,
            FRel (true, r, Bound zero_big_int, Bound unit_big_int),
            FBinop (EBConj, 
                    FEq (true, Bound zero_big_int, lift_var_multiple (big_int_of_int 2) y1), 
                    FEq (true, Bound unit_big_int, lift_var_multiple (big_int_of_int 2) y2)))
  | ISubst(y1, y2) -> failwith "transl_fof_subst of ISubst"


let rec propagate_maps rmap = function
  | AtomC(cn) -> AtomC(cn)
  | Top -> Top
  | Bottom -> Bottom
  | NotC(c) -> NotC(propagate_maps rmap c)
  | AndC(c1, c2) -> AndC(propagate_maps rmap c1, propagate_maps rmap c2)
  | OrC(c1, c2) -> OrC(propagate_maps rmap c1, propagate_maps rmap c2)
  | NumRestrC (nro, n, r, c) -> NumRestrC (nro, n, replace r rmap, propagate_maps rmap c)
  | SubstC _ -> failwith "propagate_maps: no substitutions in concepts"

let transl_fof_fact rmap imap = function
(*
  | Inst(x,c) -> transl_fof_concept (replace x imap) (propagate_maps rmap c)
*)
  | Inst(x,c) -> transl_fof_concept rmap (replace x imap) c
  | AtomR(sign, r, x, y) -> FRel(sign, replace r rmap, replace x imap, replace y imap)
  | Eq(sign, x, y) -> FEq(sign, replace x imap, replace y imap)

let rec transl_fof_form rmap imap = function
  | FalseFm -> FFalse
  | FactFm(f) ->  transl_fof_fact rmap imap f
  | NegFm(f) -> FNeg(transl_fof_form rmap imap f)
  | BinopFm(bop, f1, f2) -> 
    FBinop(binop_into_extended_binop bop, transl_fof_form rmap imap f1, transl_fof_form rmap imap f2)
  | QuantifFm(q, f) -> FQuantif(q, "", transl_fof_form rmap (lift_vmap imap) f)
  | SubstFm(f, RSubst (r, rop, (y1, y2))) ->
    let new_rsubst = RSubst (replace r rmap, rop, (replace y1 imap, replace y2 imap)) in 
    FLetR (transl_fof_subst new_rsubst, transl_fof_form ((r, Bound zero_big_int)::(lift_vmap rmap)) imap f)
  | SubstFm(f, ISubst (v, v')) -> 
    transl_fof_form rmap ((v, v')::(lift_vmap imap)) f

(* TESTS 
transl_fof_form [] []
  (SubstFm (
    SubstFm (
      SubstFm (
        SubstFm(
          BinopFm(Conj, 
                  FactFm (AtomR(true, Free "r", Free "x", Free "y")), 
                  FactFm (AtomR(false, Free "s", Free "x", Free "y"))),
          RSubst (Free "r", RDiff, (Free "y1r", Free "y2r"))),
        ISubst (Free "y1r", Free "y1rren")),
       RSubst (Free "s", RAdd, (Free "y1s", Free "y2s"))),
     RSubst (Free "r", RDiff, (Free "y3r", Free "y4r"))))
;;

transl_fof_form [] []
  (SubstFm (FactFm (AtomR(true, Free "r", Free "x", Free "y")),
            RSubst (Free "r", RDiff, (Free "y3r", Free "y4r"))))
;;
*)

      (* ---------------------------------------------------------------------- *)

(* Lift 'r and 'c variables to 'r var resp. 'c var *)

let rec create_r_c_vars_concept = function
  | AtomC(cn) -> AtomC(Free cn)
  | Top -> Top
  | Bottom -> Bottom
  | NotC(c) -> NotC(create_r_c_vars_concept c)
  | AndC(c1, c2) -> AndC(create_r_c_vars_concept c1, create_r_c_vars_concept c2)
  | OrC(c1, c2) -> OrC(create_r_c_vars_concept c1, create_r_c_vars_concept c2)
  | NumRestrC (nro, n, r, c) -> NumRestrC (nro, n, Free r, create_r_c_vars_concept c)
  | SubstC _ -> failwith "create_r_c_vars_concept: no substitutions in concepts"

let rec create_r_c_vars_fact = function
  | Inst(x,c) -> Inst(x, create_r_c_vars_concept c)
  | AtomR(sign, r, x, y) -> AtomR(sign, Free r, x, y)
  | Eq(sign, x, y) -> Eq(sign, x, y)

let rec create_r_c_vars_form = function
  | FalseFm -> FalseFm
  | FactFm(f) ->  FactFm(create_r_c_vars_fact f)
  | NegFm(f) -> NegFm(create_r_c_vars_form f)
  | BinopFm(bop, f1, f2) -> BinopFm(bop, create_r_c_vars_form f1, create_r_c_vars_form f2)
  | QuantifFm(q, f) -> QuantifFm(q, create_r_c_vars_form f)
  | SubstFm(f, RSubst (r, rop, (y1, y2))) ->
    SubstFm(create_r_c_vars_form f, RSubst (Free r, rop, (y1, y2)))
  | SubstFm(f, ISubst(y1, y2)) -> SubstFm(create_r_c_vars_form f, ISubst(y1, y2))

;;


      (* ---------------------------------------------------------------------- *)

let rec bigint_assoc n = function
  | [] -> failwith "empty assoc"
  | (k, v) :: xs -> if (eq_big_int n k) then v else bigint_assoc n xs

let name_var namemap = function 
  | Bound n -> bigint_assoc n namemap
  | Free x -> x

(* TODO: OLD
let rec away_from name i ins = 
  let suggestion = name ^ (string_of_int i) in
  if List.exists (fun (b, n) -> suggestion = n) ins
  then away_from name (i + 1) ins
  else suggestion
*)

let rec away_from name i free_names = 
  let suggestion = name ^ (string_of_big_int i) in
  if List.mem suggestion free_names
  then away_from name (succ_big_int i) free_names
  else suggestion

let remap_names incr ins = List.map (fun (i, n) -> (add_big_int i incr, n)) ins


(* ------------------------------------------------------------ *)

let eq_var x y = match (x, y) with 
  | (Free v, Free w) -> (v = w)
  | (Bound v, Bound w) -> eq_big_int v w
  | _ -> false

let rec fof_variables = function
  | FFalse -> ([], [], [])
  | FTrue -> ([], [], [])
  | FPred(c, x) -> ([], [c], [x])
  | FRel(sign, r, x, y) -> ([r], [], union eq_var [x] [y])
  | FEq(sign, x, y) -> ([], [], union eq_var [x] [y])
  | FNeg(f) -> fof_variables f
  | FBinop(bop, f1, f2) -> triple_union eq_var (fof_variables f1) (fof_variables f2)
  | FQuantif(q, _, f) -> triple_remove_bound (fof_variables f)
  | FQuantifExpl(q, _, f) -> triple_remove_bound (fof_variables f)
  | FLetC (lf, f) -> triple_remove_bound (triple_union eq_var (fof_variables lf) (fof_variables f))
  | FLetR (lf, f) -> triple_remove_bound (triple_union eq_var (fof_variables lf) (fof_variables f))


let free_name_vars vars = List.map (function Free v -> v | _ -> "") vars

let triple_free_name_vars (r, c, i) = (free_name_vars r, free_name_vars c, free_name_vars i)

let free_fof_variables f = 
  let (r, c, i) = fof_variables f in
  union (=) (union (=) (free_name_vars r) (free_name_vars c)) (free_name_vars i)

let rec name_variables rns cns ins free_names = function
  | FFalse -> (free_names, FFalse)
  | FTrue -> (free_names, FTrue)
  | FPred(c, x) -> (free_names, FPred(name_var cns c, name_var ins x))
  | FRel(sign, r, x, y) -> (free_names, FRel(sign, name_var rns r, name_var ins x, name_var ins y))
  | FEq(sign, x, y) -> (free_names, FEq(sign, name_var ins x, name_var ins y))
  | FNeg(f) -> 
    let (free_names_new, fn) = name_variables rns cns ins free_names f in
    (free_names_new, FNeg(fn))
  | FBinop(bop, f1, f2) -> 
    let (free_names1, f1n) = name_variables rns cns ins free_names f1 in
    let (free_names2, f2n) = name_variables rns cns ins free_names1 f2 in
    (free_names2, FBinop(bop, f1n, f2n))
  | FQuantif(q, _, f) -> 
    let varn = away_from "X" zero_big_int free_names in
    let (free_names_new, fn) = 
      name_variables rns cns ((zero_big_int, varn)::(remap_names unit_big_int ins)) (insert (=) varn free_names) f in
    (free_names_new, FQuantif(q, varn, fn))
  | FQuantifExpl(q, [], f) -> 
name_variables rns cns ins free_names f
  | FQuantifExpl(q, x::xs, f) -> 
    name_variables rns cns ins free_names (FQuantifExpl(q, xs, (FQuantif(q, "", f))))
  | FLetC (lf, f) -> 
    let v = away_from "V" zero_big_int free_names in
    let c = away_from "c" zero_big_int free_names in
    let (free_names_lf, lf_named) = name_variables rns cns ((zero_big_int, v)::ins) (insert (=) v (insert (=) c free_names)) lf in
    let (free_names_f, f_named)  = name_variables rns ((zero_big_int,c)::cns) ins free_names_lf f in
    (free_names_f,
     FBinop(EBImpl, 
           FQuantif(QAll, v, FBinop(EBIff, FPred(c, v), lf_named)),
             f_named))
  | FLetR (lf, f) -> 
    let v0 = away_from "V" zero_big_int free_names in
    let v1 = away_from "V" unit_big_int (v0::free_names) in
    let r = away_from "r" zero_big_int (v1::v0::free_names) in
    let (free_names_lf, lf_named) = 
      name_variables rns cns ((zero_big_int, v0)::(unit_big_int, v1)::remap_names (big_int_of_int 2) ins)
        (insert (=) v0 (insert (=) v1 (insert (=) r free_names))) lf in
    let (free_names_f, f_named) = 
      name_variables ((zero_big_int,r)::rns) cns ins free_names_lf f in
    (free_names_f, 
     FBinop(EBImpl,
            FQuantif(QAll, v0, FQuantif(QAll, v1, FBinop(EBIff, FRel(true, r, v0, v1), lf_named))),
            f_named))

      (* ---------------------------------------------------------------------- *)

(* Lift let (FLetC, FLetR) above connectors (but not above quantifiers) *)
let let_free = function
  | FLetC (lf, f) -> false
  | FLetR (lf, f) -> false
  | _ -> true


let rec lift_let = function
  | FFalse -> FFalse
  | FTrue -> FTrue
  | FPred(c, x) -> FPred(c, x)
  | FRel(sign, r, x, y) -> FRel(sign, r, x, y)
  | FEq(sign, x, y) -> FEq(sign, x, y)
  | FNeg(FLetC (lf, f)) -> FLetC (lf, lift_let (FNeg f))
  | FNeg(FLetR (lf, f)) -> FLetR (lf, lift_let (FNeg f))
  | FNeg(f) -> 
    let lift_f = lift_let f in
    if let_free lift_f 
    then FNeg (lift_f)
    else lift_let (FNeg lift_f)
  | FBinop(bop, FLetC (lf, f1), f2) -> FLetC (lf, lift_let (FBinop(bop, f1, f2)))
  | FBinop(bop, FLetR (lf, f1), f2) -> FLetR (lf, lift_let (FBinop(bop, f1, f2)))
  | FBinop(bop, f1, FLetC (lf, f2)) -> FLetC (lf, lift_let (FBinop(bop, f1, f2)))
  | FBinop(bop, f1, FLetR (lf, f2)) -> FLetR (lf, lift_let (FBinop(bop, f1, f2)))
  | FBinop(bop, f1, f2) -> 
    let lift_f1 = lift_let f1 in
    let lift_f2 = lift_let f2 in
    if let_free lift_f1 && let_free lift_f2
    then FBinop(bop, lift_f1, lift_f2)
    else lift_let (FBinop(bop, lift_f1, lift_f2))


  | FQuantif(q, n, f) -> FQuantif(q, n, lift_let f) (* no lift above quantifiers *)
  | FQuantifExpl(q, xs, f) -> FQuantifExpl(q, xs, lift_let f)
  | FLetC (lf, f) -> FLetC (lf, lift_let f)
  | FLetR (lf, f) -> FLetR (lf, lift_let f)

      (* ---------------------------------------------------------------------- *)


let print_binop f1 f2 = function
  | EBConj -> f1 ^ "&" ^ f2
  | EBDisj -> f1 ^ "|" ^ f2
  | EBImpl -> f1 ^ "=>\n" ^ f2
  | EBIff  -> f1 ^ "<=>" ^ f2

let print_quantif = function
  | QAll -> "!"
  | QEx -> "?"

let rec print_fof = function
  | FFalse -> "$false"
  | FTrue -> "$true"
  | FPred(c, x) -> c ^ "(" ^ x ^ ")"
  | FRel(sign, r, x, y) -> (if sign then "" else "~") ^ r ^ "(" ^ x ^ "," ^ y ^")"
  | FEq(sign, x, y) -> "(" ^ x ^ (if sign then "=" else "!=") ^ y ^ ")"
  | FNeg(f) -> "~ (" ^ print_fof f ^ ")"
  | FBinop(bop, f1, f2) -> "(" ^ print_binop (print_fof f1) (print_fof f2) bop ^ ")"
  | FQuantif(q, n, f) -> "(" ^ print_quantif q ^ "[" ^ n ^ "] : " ^ print_fof f ^ ")"
  | FQuantifExpl(q, _, f) -> failwith "print_fof: FQuantifExpl not implemented"
  | FLetC (lf, f) -> failwith "print_fof: FLetC not implemented"
  | FLetR (lf, f) -> failwith "print_fof: FLetR not implemented"


let extract_vcs (Prog(decls, pre, c, post)) = 
  (BinopFm (Conj, vc c post, BinopFm (Impl, pre, (wp_dl c post))))

(* conversion of verification conditions *)
let convert_vcs vcs =
  let lifted_vcs = (create_r_c_vars_form vcs) in
  let transl_vcs = transl_fof_form [] [] lifted_vcs in
  let let_lifted = lift_let transl_vcs in
  let free_vars = free_fof_variables let_lifted in
  let (free_names, named_fof) = (name_variables [] [] [] free_vars let_lifted) in
  named_fof

let print_tptp fof = "fof(prove_verif_condition, conjecture,\n" ^  (print_fof fof) ^ ")."

(* TODO: OLD
let convert_vcs vcs = 
  print_fof (name_variables [] [] [] (transl_fof_form [] [] (create_r_c_vars_form vcs)))
*)


exception ParseLexError of exn * (int * int * string * string)

let parse infile = 
  let lexbuf = Lexing.from_channel (open_in infile) in
  try 
    Parser.start Lexer.token lexbuf
  with exn ->
    begin
      let curr = lexbuf.Lexing.lex_curr_p in
      let line = curr.Lexing.pos_lnum in
      let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
      let tok = Lexing.lexeme lexbuf in
      let tail = Lexer.ruleTail "" lexbuf in
      raise (ParseLexError (exn,(line,cnum,tok,tail)))
    end
;;


let generate infile outfile = 
  let outf = open_out outfile in
  let str = print_tptp (convert_vcs (extract_vcs (parse infile))) in
  output_string outf str ; flush outf

let generate_and_prove prover_exec filename =
  let infile = filename ^ ".trans" in
  let outfile = filename ^ ".p" in
  generate infile outfile;
  Sys.command (prover_exec ^ " " ^ outfile)
;;

let main () = 
  try
    generate_and_prove Sys.argv.(1) Sys.argv.(2)
  with ParseLexError (exn,(line,cnum,tok,tail)) ->
    (print_string ("Parsing error on line: " ^ (string_of_int line) ^ 
                     " column: " ^ (string_of_int cnum) ^
                     " token: "  ^ tok ^
                     "\nrest: "  ^ tail ^ "\n")
    ;
    1)
;;

main();;



 (* 
# convert_vcs (extract_vcs (parse "Examples/ex2.trans")) ;;
 *)
