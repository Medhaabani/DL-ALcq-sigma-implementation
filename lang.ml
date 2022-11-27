open Verif_condition.VerifCondition;;
open Auxil;;


let trueFm = ConstFm(true)
let falseFm = ConstFm(false)

let andC (c1, c2) = (BinopC(Conj, c1, c2))

let orC (c1, c2) = (BinopC(Disj, c1, c2))


let conjFm (f1, f2) = BinopFm(Conj, f1, f2)
let disjFm (f1, f2) = BinopFm(Disj, f1, f2)
let implFm (f1, f2) = BinopFm(Disj, NegFm(f1), f2)

let qconjFm (f1, f2) = QBinopFm(Conj, f1, f2)
let qdisjFm (f1, f2) = QBinopFm(Disj, f1, f2)
let qimplFm (f1, f2) = QBinopFm(Disj, QNegFm(f1), f2)


type decls = Decls of ((string list) * (string list) * (string list))

type ('nr, 'nc, 'ni) prog = 
  Prog of decls * 
      ('nr, 'nc, 'ni) form * 
      ('nr, 'nc, 'ni)  stmt * 
      ('nr, 'nc, 'ni) form


let print_rolop = function
  | RDiff -> " - "
  | RAdd -> " + "

let print_binop = function
  | Conj -> "&&"
  | Disj -> "||"

(* bds: list of bound names for  [Bound 0 .. Bound n] in that order *)
let print_var bds = function
    | Free s -> " " ^ s ^ " "
    | Bound (Nat n) -> " " ^ (List.nth bds (Big_int.int_of_big_int n)) ^ " "
;;

let print_var_simple vn = " " ^ vn ^ " "
;;

let print_quantif = function
  | QAll -> "ALL "
  | QEx -> "EX "

let print_subst pv = function
  | RSubst (r, rop, (v1, v2)) -> 
    "[ " ^ r ^ " := " ^ r ^ (print_rolop rop) ^ "(" ^ (pv v1) ^ "," ^ (pv v2) ^ ") ]"
  | CSubst (c, rop, v) -> "[ " ^ c ^ " := " ^ c ^ (print_rolop rop) ^ (pv v) ^ " ]"
  | ISubst (v1, v2) -> "[ " ^ (pv v1) ^ " := " ^ (pv v2) ^ " ]"

let print_numres_ord = function
  | Lt -> "< "
  | Ge -> ">= "

let print_number n = string_of_int (Big_int.int_of_big_int (integer_of_nat n))

let rec print_concept pv = function
  | AtomC (sign, c) -> (if sign then c else "(-" ^ c ^ ")")
  | Top -> "Top"
  | Bottom -> "Bottom"
  | NegC (c) -> "(-" ^ (print_concept pv c) ^ ")"  
  | BinopC (bop, c1, c2) -> "(" ^ (print_concept pv c1) ^ (print_binop bop) ^ (print_concept pv c2) ^ ")"
  | NumRestrC (nro, n, r, c) -> 
     "(" ^ (print_numres_ord nro) ^ (print_number n) ^ r ^ (print_concept pv c)  ^ ")"
  | SubstC (c, sbst) -> "(" ^ (print_concept pv c) ^ (print_subst pv sbst) ^ ")"
  | SomeC (r, c) -> "( [?] " ^ r ^ " " ^ (print_concept pv c) ^ ")"
  | AllC (r, c) -> "( [!] " ^ r ^ " " ^ (print_concept pv c) ^ ")"

let print_fact pv = function
  | Inst (x, c) -> "(" ^ (pv x) ^ " : " ^ (print_concept pv c) ^ ")"
  | AtomR (b, r, v1, v2) -> "(" ^ (pv v1) ^ (if b then "" else " -") ^ r ^ (pv v2) ^ ")"
  | Eq (b, v1, v2) -> "(" ^ (pv v1) ^ (if b then "=" else "!=") ^ (pv v2) ^ ")"

let rec print_form = function
  | ConstFm(b) -> if b then " true " else " false "
  | FactFm(f) -> print_fact print_var_simple f
  | NegFm (f) -> "(-" ^ (print_form f) ^ ")"  
  | BinopFm (bop, f1, f2) -> "(" ^ (print_form f1) ^ (print_binop bop) ^ (print_form f2) ^ ")"
  | SubstFm (f, sbst) -> "(" ^ (print_form f) ^ (print_subst print_var_simple sbst) ^ ")"

(* uns: list of used names; bds: list of bound names for  [Bound 0 .. Bound n] *)
let rec print_qform_rec uns bds = function
  | QConstFm(b) -> if b then " true " else " false "
  | QFactFm(f) -> print_fact (print_var bds) f
  | QNegFm (f) -> "(-" ^ (print_qform_rec uns bds f) ^ ")"  
  | QBinopFm (bop, f1, f2) -> 
    "(" ^ (print_qform_rec uns bds f1) ^ (print_binop bop) ^ (print_qform_rec uns bds f2) ^ ")"
  | QuantifFm(q, v, f) -> 
    let nv = new_var_string v uns in 
    let n_uns = (if nv = v then uns else nv::uns) in
    "(" ^ (print_quantif q) ^ nv ^ "." ^ (print_qform_rec n_uns (nv::bds) f) ^ ")"
  | QSubstFm (f, sbst) -> 
    "(" ^ (print_qform_rec uns bds f) ^ (print_subst (print_var bds) sbst) ^ ")"

let print_qform f = 
  print_qform_rec (free_var_names (fv_qform_list {equal = (=)} f)) [] f
