module VerifCondition : sig
  type 'a equal = {equal : 'a -> 'a -> bool}
  val equal : 'a equal -> 'a -> 'a -> bool
  type nat = Nat of Big_int.big_int
  val integer_of_nat : nat -> Big_int.big_int
  type 'a var = Free of 'a | Bound of nat
  type role_op = RDiff | RAdd
  type ('a, 'b, 'c) subst = RSubst of 'a * role_op * ('c * 'c) |
    CSubst of 'b * role_op * 'c | ISubst of 'c * 'c
  type binop = Conj | Disj
  type numres_ord = Lt | Ge
  type ('a, 'b, 'c) concept = Top | Bottom | AtomC of bool * 'b |
    NegC of ('a, 'b, 'c) concept |
    BinopC of binop * ('a, 'b, 'c) concept * ('a, 'b, 'c) concept |
    NumRestrC of numres_ord * nat * 'a * ('a, 'b, 'c) concept |
    SubstC of ('a, 'b, 'c) concept * ('a, 'b, 'c) subst |
    SomeC of 'a * ('a, 'b, 'c) concept | AllC of 'a * ('a, 'b, 'c) concept
  type ('a, 'b, 'c) fact = Inst of 'c * ('a, 'b, 'c) concept |
    AtomR of bool * 'a * 'c * 'c | Eq of bool * 'c * 'c
  type ('a, 'b, 'c) form = ConstFm of bool | FactFm of ('a, 'b, 'c) fact |
    NegFm of ('a, 'b, 'c) form |
    BinopFm of binop * ('a, 'b, 'c) form * ('a, 'b, 'c) form |
    SubstFm of ('a, 'b, 'c) form * ('a, 'b, 'c) subst
  type 'a ord = {less_eq : 'a -> 'a -> bool; less : 'a -> 'a -> bool}
  val less_eq : 'a ord -> 'a -> 'a -> bool
  val less : 'a ord -> 'a -> 'a -> bool
  val nat_of_integer : Big_int.big_int -> nat
  type num
  val fresh_string_literal : string list -> string -> string
  type 'a new_var_list_class = {new_var_list : 'a list -> 'a -> 'a}
  val new_var_list : 'a new_var_list_class -> 'a list -> 'a -> 'a
  type quantif = QAll | QEx
  type ('a, 'b, 'c) qform = QConstFm of bool | QFactFm of ('a, 'b, 'c var) fact
    | QNegFm of ('a, 'b, 'c) qform |
    QBinopFm of binop * ('a, 'b, 'c) qform * ('a, 'b, 'c) qform |
    QuantifFm of quantif * 'c * ('a, 'b, 'c) qform |
    QSubstFm of ('a, 'b, 'c) qform * ('a, 'b, 'c var) subst
  type ('a, 'b, 'c) stmt = Skip | NAdd of 'c * 'b | NDel of 'c * 'b |
    EAdd of 'c * 'a * 'c | EDel of 'c * 'a * 'c |
    SelAss of 'c * ('a, 'b, 'c) form |
    Seq of ('a, 'b, 'c) stmt * ('a, 'b, 'c) stmt |
    If of ('a, 'b, 'c) form * ('a, 'b, 'c) stmt * ('a, 'b, 'c) stmt |
    While of ('a, 'b, 'c) form * ('a, 'b, 'c) form * ('a, 'b, 'c) stmt
  type ('a, 'b, 'c) rule_rep = Clash_rep | AndRule_rep of ('a, 'b, 'c) form |
    OrRule_rep of ('a, 'b, 'c) form | ConjRule_rep of ('a, 'b, 'c) form |
    DisjRule_rep of ('a, 'b, 'c) form | Todo_rep
  type ('a, 'b, 'c) trace =
    Trace of ('a, 'b, 'c) rule_rep * ('a, 'b, 'c) trace list
  type ('a, 'b, 'c) inactive_form =
    Inactive_form of
      (('a, 'b, 'c) form list *
        (('a, 'b, 'c) form list *
          (('a, 'b, 'c) form list *
            (('a, 'b, 'c) form list * ('a, 'b, 'c) form list))))
  type ('a, 'b, 'c) branch =
    Branch of
      (('a, 'b, 'c) form list *
        (('a, 'b, 'c) form list *
          (('a, 'b, 'c) form list *
            (('a, 'b, 'c) form list * ('a, 'b, 'c) inactive_form))))
  type ('a, 'b, 'c) apply_result =
    AppRes of nat * ('a, 'b, 'c) rule_rep option * ('a, 'b, 'c) branch list
  type ('a, 'b) proof_result = TablUnsatisf of 'a | TablSatisf of 'b
  type ('a, 'b, 'c) trace_constr = CTrace of ('a, 'b, 'c) trace |
    ITrace of ('a, 'b, 'c) trace list * nat * ('a, 'b, 'c) rule_rep
  val qform_of_form : ('a, 'b, 'c) form -> ('a, 'b, 'c) qform
  val wp_dl : ('a, 'b, 'c) stmt -> ('a, 'b, 'c) qform -> ('a, 'b, 'c) qform
  val vc : ('a, 'b, 'c) stmt -> ('a, 'b, 'c) qform -> ('a, 'b, 'c) qform
  val remdups : 'a equal -> 'a list -> 'a list
  val list_union : 'a equal -> 'a list -> 'a list -> 'a list
  val extract_vcs :
    ('a, 'b, 'c) qform ->
      ('a, 'b, 'c) stmt -> ('a, 'b, 'c) qform -> ('a, 'b, 'c) qform
  val neg_norm_concept : bool -> ('a, 'b, 'c) concept -> ('a, 'b, 'c) concept
  val neg_norm_form : bool -> ('a, 'b, 'c) form -> ('a, 'b, 'c) form
  val push_subst_form :
    'a equal -> 'b equal -> 'c equal ->
      ('a, 'b, 'c) form -> ('a, 'b, 'c) subst list -> ('a, 'b, 'c) form
  val push_isubst_form :
    'c equal -> ('a, 'b, 'c) form -> ('c * 'c) list -> ('a, 'b, 'c) form
  val fv_form_list : 'c equal -> ('a, 'b, 'c) form -> 'c list
  val fv_qform_list : 'c equal -> ('a, 'b, 'c) qform -> 'c var list
  val all_forms : ('a, 'b, 'c) branch -> ('a, 'b, 'c) form list
  val numrestrc_lt_applicable_vars :
    'a equal -> 'b equal -> 'c equal * 'c ord ->
      ('a, 'b, 'c) branch ->
        'c -> nat -> 'a -> ('a, 'b, 'c) concept -> ('c * 'c) list
  val choose_applicable_vars :
    'a equal -> 'b equal -> 'c equal ->
      ('a, 'b, 'c) branch ->
        'c -> 'a -> ('a, 'b, 'c) concept -> 'c list * 'c list
  val is_inactive :
    'a equal -> 'b equal -> 'c equal * 'c ord -> ('a, 'b, 'c) branch -> bool
  val apply_linear :
    'a equal -> 'b equal -> 'c equal * 'c new_var_list_class * 'c ord ->
      ('a, 'b, 'c) branch -> ('a, 'b, 'c) form -> ('a, 'b, 'c) apply_result
  val classify_new :
    'a equal -> 'b equal -> 'c equal ->
      ('a, 'b, 'c) branch -> ('a, 'b, 'c) branch
  val apply_splitting :
    'a equal -> 'b equal -> 'c equal ->
      ('a, 'b, 'c) branch -> ('a, 'b, 'c) form -> ('a, 'b, 'c) apply_result
  val apply_permanent :
    'a equal -> 'b equal -> 'c equal * 'c ord ->
      ('a, 'b, 'c) branch -> ('a, 'b, 'c) form -> ('a, 'b, 'c) apply_result
  val apply_rules_pt :
    'a equal -> 'b equal -> 'c equal * 'c new_var_list_class * 'c ord ->
      ('a, 'b, 'c) branch -> ('a, 'b, 'c) apply_result
  val contains_clash_branch :
    'a equal -> 'b equal -> 'c equal -> ('a, 'b, 'c) branch -> bool
  val free_prenex_form_list_string :
    ('a, 'b, string) qform -> ('a, 'b, string) form
end = struct

type 'a equal = {equal : 'a -> 'a -> bool};;
let equal _A = _A.equal;;

let rec eq _A a b = equal _A a b;;

let rec equal_lista _A
  x0 x1 = match x0, x1 with [], x21 :: x22 -> false
    | x21 :: x22, [] -> false
    | x21 :: x22, y21 :: y22 -> eq _A x21 y21 && equal_lista _A x22 y22
    | [], [] -> true;;

let rec equal_list _A = ({equal = equal_lista _A} : ('a list) equal);;

type nat = Nat of Big_int.big_int;;

let rec integer_of_nat (Nat x) = x;;

let rec equal_nat
  m n = Big_int.eq_big_int (integer_of_nat m) (integer_of_nat n);;

type 'a var = Free of 'a | Bound of nat;;

let rec equal_vara _A
  x0 x1 = match x0, x1 with Free v, Bound nat -> false
    | Bound nat, Free v -> false
    | Bound nata, Bound nat -> equal_nat nata nat
    | Free va, Free v -> eq _A va v;;

let rec equal_var _A = ({equal = equal_vara _A} : 'a var equal);;

let equal_char = ({equal = (fun a b -> ((a : char) = b))} : char equal);;

let rec equal_bool
  p pa = match p, pa with p, true -> p
    | p, false -> not p
    | true, p -> p
    | false, p -> not p;;

type role_op = RDiff | RAdd;;

let rec equal_role_op
  x0 x1 = match x0, x1 with RDiff, RAdd -> false
    | RAdd, RDiff -> false
    | RAdd, RAdd -> true
    | RDiff, RDiff -> true;;

let rec equal_prod _A _B (x1, x2) (y1, y2) = eq _A x1 y1 && eq _B x2 y2;;

type ('a, 'b, 'c) subst = RSubst of 'a * role_op * ('c * 'c) |
  CSubst of 'b * role_op * 'c | ISubst of 'c * 'c;;

let rec equal_subst _A _B _C
  x0 x1 = match x0, x1 with CSubst (nc, role_op, ni), ISubst (ni1, ni2) -> false
    | ISubst (ni1, ni2), CSubst (nc, role_op, ni) -> false
    | RSubst (nr, role_op, prod), ISubst (ni1, ni2) -> false
    | ISubst (ni1, ni2), RSubst (nr, role_op, prod) -> false
    | RSubst (nr, role_opa, prod), CSubst (nc, role_op, ni) -> false
    | CSubst (nc, role_opa, ni), RSubst (nr, role_op, prod) -> false
    | ISubst (ni1a, ni2a), ISubst (ni1, ni2) -> eq _C ni1a ni1 && eq _C ni2a ni2
    | CSubst (nca, role_opa, nia), CSubst (nc, role_op, ni) ->
        eq _B nca nc && (equal_role_op role_opa role_op && eq _C nia ni)
    | RSubst (nra, role_opa, proda), RSubst (nr, role_op, prod) ->
        eq _A nra nr &&
          (equal_role_op role_opa role_op && equal_prod _C _C proda prod);;

type binop = Conj | Disj;;

let rec equal_binop
  x0 x1 = match x0, x1 with Conj, Disj -> false
    | Disj, Conj -> false
    | Disj, Disj -> true
    | Conj, Conj -> true;;

type numres_ord = Lt | Ge;;

let rec equal_numres_ord
  x0 x1 = match x0, x1 with Lt, Ge -> false
    | Ge, Lt -> false
    | Ge, Ge -> true
    | Lt, Lt -> true;;

type ('a, 'b, 'c) concept = Top | Bottom | AtomC of bool * 'b |
  NegC of ('a, 'b, 'c) concept |
  BinopC of binop * ('a, 'b, 'c) concept * ('a, 'b, 'c) concept |
  NumRestrC of numres_ord * nat * 'a * ('a, 'b, 'c) concept |
  SubstC of ('a, 'b, 'c) concept * ('a, 'b, 'c) subst |
  SomeC of 'a * ('a, 'b, 'c) concept | AllC of 'a * ('a, 'b, 'c) concept;;

let rec equal_concept _A _B _C
  x0 x1 = match x0, x1 with SomeC (nra, concepta), AllC (nr, concept) -> false
    | AllC (nra, concepta), SomeC (nr, concept) -> false
    | SubstC (concepta, subst), AllC (nr, concept) -> false
    | AllC (nr, concepta), SubstC (concept, subst) -> false
    | SubstC (concepta, subst), SomeC (nr, concept) -> false
    | SomeC (nr, concepta), SubstC (concept, subst) -> false
    | NumRestrC (numres_ord, nat, nra, concepta), AllC (nr, concept) -> false
    | AllC (nra, concepta), NumRestrC (numres_ord, nat, nr, concept) -> false
    | NumRestrC (numres_ord, nat, nra, concepta), SomeC (nr, concept) -> false
    | SomeC (nra, concepta), NumRestrC (numres_ord, nat, nr, concept) -> false
    | NumRestrC (numres_ord, nat, nr, concepta), SubstC (concept, subst) ->
        false
    | SubstC (concepta, subst), NumRestrC (numres_ord, nat, nr, concept) ->
        false
    | BinopC (binop, concept1, concept2), AllC (nr, concept) -> false
    | AllC (nr, concept), BinopC (binop, concept1, concept2) -> false
    | BinopC (binop, concept1, concept2), SomeC (nr, concept) -> false
    | SomeC (nr, concept), BinopC (binop, concept1, concept2) -> false
    | BinopC (binop, concept1, concept2), SubstC (concept, subst) -> false
    | SubstC (concept, subst), BinopC (binop, concept1, concept2) -> false
    | BinopC (binop, concept1, concept2),
        NumRestrC (numres_ord, nat, nr, concept)
        -> false
    | NumRestrC (numres_ord, nat, nr, concept),
        BinopC (binop, concept1, concept2)
        -> false
    | NegC concepta, AllC (nr, concept) -> false
    | AllC (nr, concepta), NegC concept -> false
    | NegC concepta, SomeC (nr, concept) -> false
    | SomeC (nr, concepta), NegC concept -> false
    | NegC concepta, SubstC (concept, subst) -> false
    | SubstC (concepta, subst), NegC concept -> false
    | NegC concepta, NumRestrC (numres_ord, nat, nr, concept) -> false
    | NumRestrC (numres_ord, nat, nr, concepta), NegC concept -> false
    | NegC concept, BinopC (binop, concept1, concept2) -> false
    | BinopC (binop, concept1, concept2), NegC concept -> false
    | AtomC (boola, nc), AllC (nr, concept) -> false
    | AllC (nr, concept), AtomC (boola, nc) -> false
    | AtomC (boola, nc), SomeC (nr, concept) -> false
    | SomeC (nr, concept), AtomC (boola, nc) -> false
    | AtomC (boola, nc), SubstC (concept, subst) -> false
    | SubstC (concept, subst), AtomC (boola, nc) -> false
    | AtomC (boola, nc), NumRestrC (numres_ord, nat, nr, concept) -> false
    | NumRestrC (numres_ord, nat, nr, concept), AtomC (boola, nc) -> false
    | AtomC (boola, nc), BinopC (binop, concept1, concept2) -> false
    | BinopC (binop, concept1, concept2), AtomC (boola, nc) -> false
    | AtomC (boola, nc), NegC concept -> false
    | NegC concept, AtomC (boola, nc) -> false
    | Bottom, AllC (nr, concept) -> false
    | AllC (nr, concept), Bottom -> false
    | Bottom, SomeC (nr, concept) -> false
    | SomeC (nr, concept), Bottom -> false
    | Bottom, SubstC (concept, subst) -> false
    | SubstC (concept, subst), Bottom -> false
    | Bottom, NumRestrC (numres_ord, nat, nr, concept) -> false
    | NumRestrC (numres_ord, nat, nr, concept), Bottom -> false
    | Bottom, BinopC (binop, concept1, concept2) -> false
    | BinopC (binop, concept1, concept2), Bottom -> false
    | Bottom, NegC concept -> false
    | NegC concept, Bottom -> false
    | Bottom, AtomC (boola, nc) -> false
    | AtomC (boola, nc), Bottom -> false
    | Top, AllC (nr, concept) -> false
    | AllC (nr, concept), Top -> false
    | Top, SomeC (nr, concept) -> false
    | SomeC (nr, concept), Top -> false
    | Top, SubstC (concept, subst) -> false
    | SubstC (concept, subst), Top -> false
    | Top, NumRestrC (numres_ord, nat, nr, concept) -> false
    | NumRestrC (numres_ord, nat, nr, concept), Top -> false
    | Top, BinopC (binop, concept1, concept2) -> false
    | BinopC (binop, concept1, concept2), Top -> false
    | Top, NegC concept -> false
    | NegC concept, Top -> false
    | Top, AtomC (boola, nc) -> false
    | AtomC (boola, nc), Top -> false
    | Top, Bottom -> false
    | Bottom, Top -> false
    | AllC (nra, concepta), AllC (nr, concept) ->
        eq _A nra nr && equal_concept _A _B _C concepta concept
    | SomeC (nra, concepta), SomeC (nr, concept) ->
        eq _A nra nr && equal_concept _A _B _C concepta concept
    | SubstC (concepta, substa), SubstC (concept, subst) ->
        equal_concept _A _B _C concepta concept &&
          equal_subst _A _B _C substa subst
    | NumRestrC (numres_orda, nata, nra, concepta),
        NumRestrC (numres_ord, nat, nr, concept)
        -> equal_numres_ord numres_orda numres_ord &&
             (equal_nat nata nat &&
               (eq _A nra nr && equal_concept _A _B _C concepta concept))
    | BinopC (binopa, concept1a, concept2a), BinopC (binop, concept1, concept2)
        -> equal_binop binopa binop &&
             (equal_concept _A _B _C concept1a concept1 &&
               equal_concept _A _B _C concept2a concept2)
    | NegC concepta, NegC concept -> equal_concept _A _B _C concepta concept
    | AtomC (boolaa, nca), AtomC (boola, nc) ->
        equal_bool boolaa boola && eq _B nca nc
    | Bottom, Bottom -> true
    | Top, Top -> true;;

type ('a, 'b, 'c) fact = Inst of 'c * ('a, 'b, 'c) concept |
  AtomR of bool * 'a * 'c * 'c | Eq of bool * 'c * 'c;;

let rec equal_fact _A _B _C
  x0 x1 = match x0, x1 with
    AtomR (boolaa, nr, ni1a, ni2a), Eq (boola, ni1, ni2) -> false
    | Eq (boolaa, ni1a, ni2a), AtomR (boola, nr, ni1, ni2) -> false
    | Inst (ni, concept), Eq (boola, ni1, ni2) -> false
    | Eq (boola, ni1, ni2), Inst (ni, concept) -> false
    | Inst (ni, concept), AtomR (boola, nr, ni1, ni2) -> false
    | AtomR (boola, nr, ni1, ni2), Inst (ni, concept) -> false
    | Eq (boolaa, ni1a, ni2a), Eq (boola, ni1, ni2) ->
        equal_bool boolaa boola && (eq _C ni1a ni1 && eq _C ni2a ni2)
    | AtomR (boolaa, nra, ni1a, ni2a), AtomR (boola, nr, ni1, ni2) ->
        equal_bool boolaa boola &&
          (eq _A nra nr && (eq _C ni1a ni1 && eq _C ni2a ni2))
    | Inst (nia, concepta), Inst (ni, concept) ->
        eq _C nia ni && equal_concept _A _B _C concepta concept;;

type ('a, 'b, 'c) form = ConstFm of bool | FactFm of ('a, 'b, 'c) fact |
  NegFm of ('a, 'b, 'c) form |
  BinopFm of binop * ('a, 'b, 'c) form * ('a, 'b, 'c) form |
  SubstFm of ('a, 'b, 'c) form * ('a, 'b, 'c) subst;;

let rec equal_forma _A _B _C
  x0 x1 = match x0, x1 with
    BinopFm (binop, form1, form2), SubstFm (form, subst) -> false
    | SubstFm (form, subst), BinopFm (binop, form1, form2) -> false
    | NegFm forma, SubstFm (form, subst) -> false
    | SubstFm (forma, subst), NegFm form -> false
    | NegFm form, BinopFm (binop, form1, form2) -> false
    | BinopFm (binop, form1, form2), NegFm form -> false
    | FactFm fact, SubstFm (form, subst) -> false
    | SubstFm (form, subst), FactFm fact -> false
    | FactFm fact, BinopFm (binop, form1, form2) -> false
    | BinopFm (binop, form1, form2), FactFm fact -> false
    | FactFm fact, NegFm form -> false
    | NegFm form, FactFm fact -> false
    | ConstFm boola, SubstFm (form, subst) -> false
    | SubstFm (form, subst), ConstFm boola -> false
    | ConstFm boola, BinopFm (binop, form1, form2) -> false
    | BinopFm (binop, form1, form2), ConstFm boola -> false
    | ConstFm boola, NegFm form -> false
    | NegFm form, ConstFm boola -> false
    | ConstFm boola, FactFm fact -> false
    | FactFm fact, ConstFm boola -> false
    | SubstFm (forma, substa), SubstFm (form, subst) ->
        equal_forma _A _B _C forma form && equal_subst _A _B _C substa subst
    | BinopFm (binopa, form1a, form2a), BinopFm (binop, form1, form2) ->
        equal_binop binopa binop &&
          (equal_forma _A _B _C form1a form1 &&
            equal_forma _A _B _C form2a form2)
    | NegFm forma, NegFm form -> equal_forma _A _B _C forma form
    | FactFm facta, FactFm fact -> equal_fact _A _B _C facta fact
    | ConstFm boolaa, ConstFm boola -> equal_bool boolaa boola;;

let rec equal_form _A _B _C =
  ({equal = equal_forma _A _B _C} : ('a, 'b, 'c) form equal);;

let equal_literal = ({equal = (fun a b -> ((a : string) = b))} : string equal);;

let rec map
  fi x1 = match fi, x1 with fi, [] -> []
    | fi, x21a :: x22 -> fi x21a :: map fi x22;;

let rec remove1 _A
  x xa1 = match x, xa1 with x, [] -> []
    | x, y :: xs -> (if eq _A x y then xs else y :: remove1 _A x xs);;

let rec less_eq_nat
  m n = Big_int.le_big_int (integer_of_nat m) (integer_of_nat n);;

type 'a ord = {less_eq : 'a -> 'a -> bool; less : 'a -> 'a -> bool};;
let less_eq _A = _A.less_eq;;
let less _A = _A.less;;

let rec max _A a b = (if less_eq _A a b then b else a);;

let ord_integer =
  ({less_eq = Big_int.le_big_int; less = Big_int.lt_big_int} :
    Big_int.big_int ord);;

let rec nat_of_integer k = Nat (max ord_integer Big_int.zero_big_int k);;

let rec plus_nat
  m n = Nat (Big_int.add_big_int (integer_of_nat m) (integer_of_nat n));;

let rec less_nat
  m n = Big_int.lt_big_int (integer_of_nat m) (integer_of_nat n);;

type num = One | Bit0 of num | Bit1 of num;;

let one_nat : nat = Nat (Big_int.big_int_of_int 1);;

type nibble = Nibble0 | Nibble1 | Nibble2 | Nibble3 | Nibble4 | Nibble5 |
  Nibble6 | Nibble7 | Nibble8 | Nibble9 | NibbleA | NibbleB | NibbleC | NibbleD
  | NibbleE | NibbleF;;

let rec comp f g = (fun x -> f (g x));;

let rec nat_of_char
  x = comp nat_of_integer (fun a -> Big_int.big_int_of_int (Char.code a)) x;;

let rec char_of_nat
  x = comp (fun a -> Char.chr (Big_int.int_of_big_int a)) integer_of_nat x;;

let rec null = function [] -> true
               | x :: xs -> false;;

let rec butlast
  = function [] -> []
    | x :: xs -> (if null xs then [] else x :: butlast xs);;

let rec last (x :: xs) = (if null xs then x else last xs);;

let rec upChar
  y = (if not (null y) &&
            (less_eq_nat (nat_of_integer (Big_int.big_int_of_int 97))
               (nat_of_char (last y)) &&
              less_nat (nat_of_char (last y))
                (nat_of_integer (Big_int.big_int_of_int 122)))
        then butlast y @ [char_of_nat (plus_nat (nat_of_char (last y)) one_nat)]
        else y @ ['a']);;

let rec member _A
  x0 y = match x0, y with [], y -> false
    | x :: xs, y -> eq _A x y || member _A xs y;;

let rec fresh
  xs y =
    (if member (equal_list equal_char) xs y
      then fresh (remove1 (equal_list equal_char) y xs) (upChar y) else y);;

let rec fresh_string_literal
  strs str =
    (let l = (fresh
               (map (fun a ->
                      (let s = a in let rec exp i l = if i < 0 then l else exp (i - 1) (String.get s i :: l) in exp (String.length s - 1) []))
                 strs)
               (let s = str in let rec exp i l = if i < 0 then l else exp (i - 1) (String.get s i :: l) in exp (String.length s - 1) [])) in let res = String.create (List.length l) in let rec imp i = function | [] -> res | c :: l -> String.set res i c; imp (i + 1) l in imp 0 l);;

let rec new_var_list_literal x = fresh_string_literal x;;

type 'a new_var_list_class = {new_var_list : 'a list -> 'a -> 'a};;
let new_var_list _A = _A.new_var_list;;

let new_var_list_class_literal =
  ({new_var_list = new_var_list_literal} : string new_var_list_class);;

type quantif = QAll | QEx;;

type ('a, 'b, 'c) qform = QConstFm of bool | QFactFm of ('a, 'b, 'c var) fact |
  QNegFm of ('a, 'b, 'c) qform |
  QBinopFm of binop * ('a, 'b, 'c) qform * ('a, 'b, 'c) qform |
  QuantifFm of quantif * 'c * ('a, 'b, 'c) qform |
  QSubstFm of ('a, 'b, 'c) qform * ('a, 'b, 'c var) subst;;

type ('a, 'b, 'c) stmt = Skip | NAdd of 'c * 'b | NDel of 'c * 'b |
  EAdd of 'c * 'a * 'c | EDel of 'c * 'a * 'c | SelAss of 'c * ('a, 'b, 'c) form
  | Seq of ('a, 'b, 'c) stmt * ('a, 'b, 'c) stmt |
  If of ('a, 'b, 'c) form * ('a, 'b, 'c) stmt * ('a, 'b, 'c) stmt |
  While of ('a, 'b, 'c) form * ('a, 'b, 'c) form * ('a, 'b, 'c) stmt;;

type ('a, 'b, 'c) rule_rep = Clash_rep | AndRule_rep of ('a, 'b, 'c) form |
  OrRule_rep of ('a, 'b, 'c) form | ConjRule_rep of ('a, 'b, 'c) form |
  DisjRule_rep of ('a, 'b, 'c) form | Todo_rep;;

type ('a, 'b, 'c) trace =
  Trace of ('a, 'b, 'c) rule_rep * ('a, 'b, 'c) trace list;;

type ('a, 'b, 'c) inactive_form =
  Inactive_form of
    (('a, 'b, 'c) form list *
      (('a, 'b, 'c) form list *
        (('a, 'b, 'c) form list *
          (('a, 'b, 'c) form list * ('a, 'b, 'c) form list))));;

type ('a, 'b, 'c) branch =
  Branch of
    (('a, 'b, 'c) form list *
      (('a, 'b, 'c) form list *
        (('a, 'b, 'c) form list *
          (('a, 'b, 'c) form list * ('a, 'b, 'c) inactive_form))));;

type ('a, 'b, 'c) apply_result =
  AppRes of nat * ('a, 'b, 'c) rule_rep option * ('a, 'b, 'c) branch list;;

type ('a, 'b) proof_result = TablUnsatisf of 'a | TablSatisf of 'b;;

type ('a, 'b, 'c) trace_constr = CTrace of ('a, 'b, 'c) trace |
  ITrace of ('a, 'b, 'c) trace list * nat * ('a, 'b, 'c) rule_rep;;

let rec id x = (fun xa -> xa) x;;

let rec suc n = plus_nat n one_nat;;

let rec apply_var_subst
  f x1 = match f, x1 with
    f, RSubst (r, rop, (v1, v2)) -> RSubst (r, rop, (f v1, f v2))
    | f, CSubst (c, rop, v) -> CSubst (c, rop, f v)
    | f, ISubst (va, v) -> ISubst (f va, f v);;

let rec apply_var_concept
  f x1 = match f, x1 with f, Bottom -> Bottom
    | f, Top -> Top
    | f, AtomC (sign, a) -> AtomC (sign, a)
    | f, BinopC (bop, c1, c2) ->
        BinopC (bop, apply_var_concept f c1, apply_var_concept f c2)
    | f, NegC c -> NegC (apply_var_concept f c)
    | f, NumRestrC (nro, nb, r, c) ->
        NumRestrC (nro, nb, r, apply_var_concept f c)
    | f, SubstC (c, sb) -> SubstC (apply_var_concept f c, apply_var_subst f sb)
    | f, SomeC (r, c) -> SomeC (r, apply_var_concept f c)
    | f, AllC (r, c) -> AllC (r, apply_var_concept f c);;

let rec apply_var_fact
  f x1 = match f, x1 with f, Inst (x, c) -> Inst (f x, apply_var_concept f c)
    | f, AtomR (sign, r, x, y) -> AtomR (sign, r, f x, f y)
    | f, Eq (sign, x, y) -> Eq (sign, f x, f y);;

let rec qform_of_form
  = function ConstFm cn -> QConstFm cn
    | FactFm fct -> QFactFm (apply_var_fact (fun a -> Free a) fct)
    | NegFm f -> QNegFm (qform_of_form f)
    | BinopFm (bop, f1, f2) ->
        QBinopFm (bop, qform_of_form f1, qform_of_form f2)
    | SubstFm (f, sb) ->
        QSubstFm (qform_of_form f, apply_var_subst (fun a -> Free a) sb);;

let rec qIfThenElseFm
  c a b =
    QBinopFm
      (Conj, QBinopFm (Disj, QNegFm c, a),
        QBinopFm (Disj, QNegFm (QNegFm c), b));;

let zero_nat : nat = Nat Big_int.zero_big_int;;

let rec lift_var
  n x1 = match n, x1 with n, Free v -> Free v
    | n, Bound k ->
        (if less_eq_nat n k then Bound (plus_nat k one_nat) else Bound k);;

let rec lift_subst
  n x1 = match n, x1 with
    n, RSubst (r, rop, (v1, v2)) ->
      RSubst (r, rop, (lift_var n v1, lift_var n v2))
    | n, CSubst (c, rop, v) -> CSubst (c, rop, lift_var n v)
    | n, ISubst (va, v) -> ISubst (lift_var n va, lift_var n v);;

let rec lift_concept
  n x1 = match n, x1 with n, Bottom -> Bottom
    | n, Top -> Top
    | n, AtomC (sign, a) -> AtomC (sign, a)
    | n, BinopC (bop, c1, c2) ->
        BinopC (bop, lift_concept n c1, lift_concept n c2)
    | n, NegC c -> NegC (lift_concept n c)
    | n, NumRestrC (nro, nb, r, c) -> NumRestrC (nro, nb, r, lift_concept n c)
    | n, SubstC (c, sb) -> SubstC (lift_concept n c, lift_subst n sb)
    | n, SomeC (r, c) -> SomeC (r, lift_concept n c)
    | n, AllC (r, c) -> AllC (r, lift_concept n c);;

let rec lift_fact
  n x1 = match n, x1 with
    n, Inst (x, c) -> Inst (lift_var n x, lift_concept n c)
    | n, AtomR (sign, r, x, y) -> AtomR (sign, r, lift_var n x, lift_var n y)
    | n, Eq (sign, x, y) -> Eq (sign, lift_var n x, lift_var n y);;

let rec lift_form
  n x1 = match n, x1 with n, QConstFm cn -> QConstFm cn
    | n, QFactFm fct -> QFactFm (lift_fact n fct)
    | n, QNegFm f -> QNegFm (lift_form n f)
    | n, QBinopFm (bop, f1, f2) ->
        QBinopFm (bop, lift_form n f1, lift_form n f2)
    | n, QuantifFm (q, v, f) ->
        QuantifFm (q, v, lift_form (plus_nat n one_nat) f)
    | n, QSubstFm (f, sb) -> QSubstFm (lift_form n f, lift_subst n sb);;

let rec bind
  q v f =
    QuantifFm
      (q, v, QSubstFm (lift_form zero_nat f, ISubst (Free v, Bound zero_nat)));;

let rec wp_dl
  x0 qd = match x0, qd with Skip, qd -> qd
    | NAdd (v, c), qd -> QSubstFm (qd, CSubst (c, RAdd, Free v))
    | NDel (v, c), qd -> QSubstFm (qd, CSubst (c, RDiff, Free v))
    | EAdd (v1, r, v2), qd ->
        QSubstFm (qd, RSubst (r, RAdd, (Free v1, Free v2)))
    | EDel (v1, r, v2), qd ->
        QSubstFm (qd, RSubst (r, RDiff, (Free v1, Free v2)))
    | SelAss (v, b), qd ->
        bind QAll v (QBinopFm (Disj, QNegFm (qform_of_form b), qd))
    | Seq (c_1, c_2), qd -> wp_dl c_1 (wp_dl c_2 qd)
    | If (b, c_1, c_2), qd ->
        qIfThenElseFm (qform_of_form b) (wp_dl c_1 qd) (wp_dl c_2 qd)
    | While (iv, b, c), qd -> qform_of_form iv;;

let rec vc
  x0 qd = match x0, qd with Skip, qd -> QConstFm true
    | NAdd (v, c), qd -> QConstFm true
    | NDel (v, c), qd -> QConstFm true
    | EAdd (v1, r, v2), qd -> QConstFm true
    | EDel (v1, r, v2), qd -> QConstFm true
    | SelAss (v, b), qd -> QConstFm true
    | Seq (c_1, c_2), qd -> QBinopFm (Conj, vc c_1 (wp_dl c_2 qd), vc c_2 qd)
    | If (b, c_1, c_2), qd -> QBinopFm (Conj, vc c_1 qd, vc c_2 qd)
    | While (iv, b, c), qd ->
        let qb = qform_of_form b in
        let qiv = qform_of_form iv in
        QBinopFm
          (Conj,
            QBinopFm
              (Conj,
                QBinopFm (Disj, QNegFm (QBinopFm (Conj, qiv, QNegFm qb)), qd),
                QBinopFm
                  (Disj, QNegFm (QBinopFm (Conj, qiv, qb)), wp_dl c qiv)),
            vc c qiv);;

let rec upt i j = (if less_nat i j then i :: upt (suc i) j else []);;

let rec zip
  xs ys = match xs, ys with x :: xs, y :: ys -> (x, y) :: zip xs ys
    | xs, [] -> []
    | [], ys -> [];;

let rec fold
  f x1 s = match f, x1, s with f, x :: xs, s -> fold f xs (f x s)
    | f, [], s -> s;;

let rec maps
  f x1 = match f, x1 with f, [] -> []
    | f, x :: xs -> f x @ maps f xs;;

let rec foldl
  f a x2 = match f, a, x2 with f, a, [] -> a
    | f, a, x :: xs -> foldl f (f a x) xs;;

let rec foldr
  f x1 = match f, x1 with f, [] -> id
    | f, x :: xs -> comp (f x) (foldr f xs);;

let rec insert _A x xs = (if member _A xs x then xs else x :: xs);;

let rec union _A = fold (insert _A);;

let rec filter
  p x1 = match p, x1 with p, [] -> []
    | p, x :: xs -> (if p x then x :: filter p xs else filter p xs);;

let rec list_ex
  p x1 = match p, x1 with p, [] -> false
    | p, x :: xs -> p x || list_ex p xs;;

let rec remdups _A
  = function [] -> []
    | x :: xs ->
        (if member _A xs x then remdups _A xs else x :: remdups _A xs);;

let rec pred_list
  p x1 = match p, x1 with p, [] -> true
    | p, x :: xs -> p x && pred_list p xs;;

let rec removeAll _A
  x xa1 = match x, xa1 with x, [] -> []
    | x, y :: xs ->
        (if eq _A x y then removeAll _A x xs else y :: removeAll _A x xs);;

let rec gen_length
  n x1 = match n, x1 with n, x :: xs -> gen_length (suc n) xs
    | n, [] -> n;;

let rec list_union _A xs ys = remdups _A (xs @ ys);;

let rec replace_var _A v1 v2 w = (if eq _A w v1 then v2 else w);;

let rec subst_vars _A
  xa0 x = match xa0, x with [], x -> x
    | (v1, v2) :: sbsts, x -> subst_vars _A sbsts (replace_var _A v1 v2 x);;

let rec list_inters _A xs ys = filter (member _A ys) xs;;

let rec list_remove _A xs ys = foldr (removeAll _A) ys xs;;

let rec extract_vcs
  p c q = QBinopFm (Conj, vc c q, QBinopFm (Disj, QNegFm p, wp_dl c q));;

let rec dual_binop
  sign x1 = match sign, x1 with sign, Conj -> (if sign then Conj else Disj)
    | sign, Disj -> (if sign then Disj else Conj);;

let rec extract_subst
  = function Inst (x, SubstC (c, sb)) -> Some (x, (c, sb))
    | Inst (v, Top) -> None
    | Inst (v, Bottom) -> None
    | Inst (v, AtomC (vb, vc)) -> None
    | Inst (v, NegC vb) -> None
    | Inst (v, BinopC (vb, vc, vd)) -> None
    | Inst (v, NumRestrC (vb, vc, vd, ve)) -> None
    | Inst (v, SomeC (vb, vc)) -> None
    | Inst (v, AllC (vb, vc)) -> None
    | AtomR (v, va, vb, vc) -> None
    | Eq (v, va, vb) -> None;;

let rec ifThenElseFm
  c a b =
    BinopFm
      (Conj, BinopFm (Disj, NegFm c, a), BinopFm (Disj, NegFm (NegFm c), b));;

let rec dual_quantif
  sign x1 = match sign, x1 with sign, QAll -> (if sign then QAll else QEx)
    | sign, QEx -> (if sign then QEx else QAll);;

let rec lift_bound_above_substfm
  x0 sb = match x0, sb with
    QuantifFm (q, v, f), sb ->
      QuantifFm (q, v, lift_bound_above_substfm f (lift_subst zero_nat sb))
    | QConstFm v, sb -> QSubstFm (QConstFm v, sb)
    | QFactFm v, sb -> QSubstFm (QFactFm v, sb)
    | QNegFm v, sb -> QSubstFm (QNegFm v, sb)
    | QBinopFm (v, va, vb), sb -> QSubstFm (QBinopFm (v, va, vb), sb)
    | QSubstFm (v, va), sb -> QSubstFm (QSubstFm (v, va), sb);;

let rec lift_bound_above_negfm
  = function
    QuantifFm (q, v, f) ->
      QuantifFm (dual_quantif false q, v, lift_bound_above_negfm f)
    | QConstFm v -> QNegFm (QConstFm v)
    | QFactFm v -> QNegFm (QFactFm v)
    | QNegFm v -> QNegFm (QNegFm v)
    | QBinopFm (v, va, vb) -> QNegFm (QBinopFm (v, va, vb))
    | QSubstFm (v, va) -> QNegFm (QSubstFm (v, va));;

let rec shuffle_right
  bop f1 x2 = match bop, f1, x2 with
    bop, f1, QuantifFm (q, v, f2) ->
      QuantifFm (q, v, shuffle_right bop (lift_form zero_nat f1) f2)
    | bop, f1, QConstFm v -> QBinopFm (bop, f1, QConstFm v)
    | bop, f1, QFactFm v -> QBinopFm (bop, f1, QFactFm v)
    | bop, f1, QNegFm v -> QBinopFm (bop, f1, QNegFm v)
    | bop, f1, QBinopFm (v, va, vb) -> QBinopFm (bop, f1, QBinopFm (v, va, vb))
    | bop, f1, QSubstFm (v, va) -> QBinopFm (bop, f1, QSubstFm (v, va));;

let rec shuffle_left
  bop x1 f2 = match bop, x1, f2 with
    bop, QuantifFm (q, v, f1), f2 ->
      QuantifFm (q, v, shuffle_left bop f1 (lift_form zero_nat f2))
    | bop, QConstFm v, f2 -> shuffle_right bop (QConstFm v) f2
    | bop, QFactFm v, f2 -> shuffle_right bop (QFactFm v) f2
    | bop, QNegFm v, f2 -> shuffle_right bop (QNegFm v) f2
    | bop, QBinopFm (v, va, vb), f2 ->
        shuffle_right bop (QBinopFm (v, va, vb)) f2
    | bop, QSubstFm (v, va), f2 -> shuffle_right bop (QSubstFm (v, va)) f2;;

let rec lift_bound
  = function QConstFm cn -> QConstFm cn
    | QFactFm fct -> QFactFm fct
    | QNegFm f -> lift_bound_above_negfm (lift_bound f)
    | QBinopFm (bop, f1, f2) -> shuffle_left bop (lift_bound f1) (lift_bound f2)
    | QuantifFm (q, v, f) -> QuantifFm (q, v, lift_bound f)
    | QSubstFm (f, sb) -> lift_bound_above_substfm (lift_bound f) sb;;

let rec fst (x1, x2) = x1;;

let rec minus_nat
  m n = Nat (max ord_integer Big_int.zero_big_int
              (Big_int.sub_big_int (integer_of_nat m) (integer_of_nat n)));;

let rec dual_numres_ord
  sign x1 = match sign, x1 with sign, Lt -> (if sign then Lt else Ge)
    | sign, Ge -> (if sign then Ge else Lt);;

let rec interp_sign sign b = (if sign then b else not b);;

let rec neg_norm_concept
  sign x1 = match sign, x1 with sign, Bottom -> (if sign then Bottom else Top)
    | sign, Top -> (if sign then Top else Bottom)
    | sign, AtomC (asign, a) -> AtomC (interp_sign sign asign, a)
    | sign, BinopC (bop, c1, c2) ->
        BinopC
          (dual_binop sign bop, neg_norm_concept sign c1,
            neg_norm_concept sign c2)
    | sign, NegC c -> neg_norm_concept (not sign) c
    | sign, NumRestrC (nro, n, r, c) ->
        NumRestrC (dual_numres_ord sign nro, n, r, neg_norm_concept true c)
    | sign, SubstC (c, sb) -> SubstC (neg_norm_concept sign c, sb)
    | sign, SomeC (r, c) ->
        (if sign then SomeC (r, neg_norm_concept sign c)
          else AllC (r, neg_norm_concept sign c))
    | sign, AllC (r, c) ->
        (if sign then AllC (r, neg_norm_concept sign c)
          else SomeC (r, neg_norm_concept sign c));;

let rec neg_norm_fact
  sign x1 = match sign, x1 with
    sign, Inst (x, c) -> Inst (x, neg_norm_concept sign c)
    | signa, AtomR (sign, r, x, y) -> AtomR (interp_sign signa sign, r, x, y)
    | signa, Eq (sign, x, y) -> Eq (interp_sign signa sign, x, y);;

let rec neg_norm_form
  sign x1 = match sign, x1 with
    sign, ConstFm cn -> ConstFm (interp_sign sign cn)
    | sign, FactFm f -> FactFm (neg_norm_fact sign f)
    | sign, NegFm f -> neg_norm_form (not sign) f
    | sign, BinopFm (bop, f1, f2) ->
        BinopFm
          (dual_binop sign bop, neg_norm_form sign f1, neg_norm_form sign f2)
    | sign, SubstFm (f, sb) -> SubstFm (neg_norm_form sign f, sb);;

let rec nnf_IfThenElseFm c a b = neg_norm_form true (ifThenElseFm c a b);;

let rec push_rsubst_concept_numrestrc
  xa0 xa1 x nro n r c = match xa0, xa1, x, nro, n, r, c with
    RDiff, (v1, v2), x, nro, n, r, c ->
      nnf_IfThenElseFm
        (BinopFm
          (Conj,
            BinopFm
              (Conj, FactFm (Eq (true, x, v1)),
                FactFm (Inst (v2, SubstC (c, RSubst (r, RDiff, (v1, v2)))))),
            FactFm (AtomR (true, r, v1, v2))))
        (FactFm
          (Inst (x, NumRestrC
                      (nro, suc n, r,
                        SubstC (c, RSubst (r, RDiff, (v1, v2)))))))
        (FactFm
          (Inst (x, NumRestrC
                      (nro, n, r, SubstC (c, RSubst (r, RDiff, (v1, v2)))))))
    | RAdd, (v1, v2), x, Lt, n, r, c ->
        (if equal_nat n zero_nat then ConstFm false
          else nnf_IfThenElseFm
                 (BinopFm
                   (Conj,
                     BinopFm
                       (Conj, FactFm (Eq (true, x, v1)),
                         FactFm
                           (Inst (v2, SubstC (c, RSubst (r, RAdd, (v1, v2)))))),
                     FactFm (AtomR (false, r, v1, v2))))
                 (FactFm
                   (Inst (x, NumRestrC
                               (Lt, minus_nat n one_nat, r,
                                 SubstC (c, RSubst (r, RAdd, (v1, v2)))))))
                 (FactFm
                   (Inst (x, NumRestrC
                               (Lt, suc (minus_nat n one_nat), r,
                                 SubstC (c, RSubst (r, RAdd, (v1, v2))))))))
    | RAdd, (v1, v2), x, Ge, n, r, c ->
        (if equal_nat n zero_nat then ConstFm true
          else nnf_IfThenElseFm
                 (BinopFm
                   (Conj,
                     BinopFm
                       (Conj, FactFm (Eq (true, x, v1)),
                         FactFm
                           (Inst (v2, SubstC (c, RSubst (r, RAdd, (v1, v2)))))),
                     FactFm (AtomR (false, r, v1, v2))))
                 (FactFm
                   (Inst (x, NumRestrC
                               (Ge, minus_nat n one_nat, r,
                                 SubstC (c, RSubst (r, RAdd, (v1, v2)))))))
                 (FactFm
                   (Inst (x, NumRestrC
                               (Ge, suc (minus_nat n one_nat), r,
                                 SubstC (c, RSubst (r, RAdd, (v1, v2))))))));;

let rec push_rsubst_concept_somec
  xa0 xa1 x r c = match xa0, xa1, x, r, c with
    RDiff, (v1, v2), x, r, c ->
      nnf_IfThenElseFm
        (BinopFm
          (Conj,
            BinopFm
              (Conj, FactFm (Eq (true, x, v1)),
                FactFm (Inst (v2, SubstC (c, RSubst (r, RDiff, (v1, v2)))))),
            FactFm (AtomR (true, r, v1, v2))))
        (FactFm
          (Inst (x, NumRestrC
                      (Ge, suc (suc zero_nat), r,
                        SubstC (c, RSubst (r, RDiff, (v1, v2)))))))
        (FactFm (Inst (x, SomeC (r, SubstC (c, RSubst (r, RDiff, (v1, v2)))))))
    | RAdd, (v1, v2), x, r, c ->
        nnf_IfThenElseFm
          (BinopFm
            (Conj,
              BinopFm
                (Conj, FactFm (Eq (true, x, v1)),
                  FactFm (Inst (v2, SubstC (c, RSubst (r, RAdd, (v1, v2)))))),
              FactFm (AtomR (false, r, v1, v2))))
          (ConstFm true)
          (FactFm
            (Inst (x, SomeC (r, SubstC (c, RSubst (r, RAdd, (v1, v2)))))));;

let rec push_rsubst_concept_allc
  xa0 xa1 x r c = match xa0, xa1, x, r, c with
    RDiff, (v1, v2), x, r, c ->
      nnf_IfThenElseFm
        (BinopFm
          (Conj,
            BinopFm
              (Conj, FactFm (Eq (true, x, v1)),
                FactFm
                  (Inst (v2, NegC (SubstC (c, RSubst (r, RDiff, (v1, v2))))))),
            FactFm (AtomR (true, r, v1, v2))))
        (FactFm
          (Inst (x, NumRestrC
                      (Lt, suc one_nat, r,
                        NegC (SubstC (c, RSubst (r, RDiff, (v1, v2))))))))
        (FactFm (Inst (x, AllC (r, SubstC (c, RSubst (r, RDiff, (v1, v2)))))))
    | RAdd, (v1, v2), x, r, c ->
        nnf_IfThenElseFm
          (BinopFm
            (Conj,
              BinopFm
                (Conj, FactFm (Eq (true, x, v1)),
                  FactFm
                    (Inst (v2, NegC (SubstC (c, RSubst (r, RAdd, (v1, v2))))))),
              FactFm (AtomR (false, r, v1, v2))))
          (ConstFm false)
          (FactFm
            (Inst (x, AllC (r, SubstC (c, RSubst (r, RAdd, (v1, v2)))))));;

let rec push_rsubst_concept _A
  r rop v1v2 x xa4 = match r, rop, v1v2, x, xa4 with
    r, rop, v1v2, x, AtomC (sign, a) -> FactFm (Inst (x, AtomC (sign, a)))
    | r, rop, v1v2, x, Top -> FactFm (Inst (x, Top))
    | r, rop, v1v2, x, Bottom -> FactFm (Inst (x, Bottom))
    | r, rop, v1v2, x, NegC c ->
        FactFm (Inst (x, NegC (SubstC (c, RSubst (r, rop, v1v2)))))
    | r, rop, v1v2, x, BinopC (bop, c1, c2) ->
        FactFm
          (Inst (x, BinopC
                      (bop, SubstC (c1, RSubst (r, rop, v1v2)),
                        SubstC (c2, RSubst (r, rop, v1v2)))))
    | ra, rop, v1v2, x, NumRestrC (nro, n, r, c) ->
        (if eq _A ra r then push_rsubst_concept_numrestrc rop v1v2 x nro n ra c
          else FactFm
                 (Inst (x, NumRestrC
                             (nro, n, r, SubstC (c, RSubst (ra, rop, v1v2))))))
    | r, rop, v1v2, x, SubstC (c, sb) ->
        SubstFm (FactFm (Inst (x, SubstC (c, sb))), RSubst (r, rop, v1v2))
    | ra, rop, v1v2, x, SomeC (r, c) ->
        (if eq _A ra r then push_rsubst_concept_somec rop v1v2 x ra c
          else FactFm (Inst (x, SomeC (r, SubstC (c, RSubst (ra, rop, v1v2))))))
    | ra, rop, v1v2, x, AllC (r, c) ->
        (if eq _A ra r then push_rsubst_concept_allc rop v1v2 x ra c
          else FactFm
                 (Inst (x, AllC (r, SubstC (c, RSubst (ra, rop, v1v2))))));;

let rec subst_AtomR_RDiff
  sign r x y v1 v2 =
    neg_norm_form sign
      (BinopFm
        (Conj,
          BinopFm
            (Disj, FactFm (Eq (false, x, v1)), FactFm (Eq (false, y, v2))),
          FactFm (AtomR (true, r, x, y))));;

let rec subst_AtomR_RAdd
  sign r x y v1 v2 =
    neg_norm_form sign
      (BinopFm
        (Disj,
          BinopFm (Conj, FactFm (Eq (true, x, v1)), FactFm (Eq (true, y, v2))),
          FactFm (AtomR (true, r, x, y))));;

let rec push_rsubst_fact _A
  r rop v1v2 x3 = match r, rop, v1v2, x3 with
    r, rop, v1v2, Inst (x, c) -> push_rsubst_concept _A r rop v1v2 x c
    | ra, rop, v1v2, AtomR (sign, r, x, y) ->
        (if eq _A ra r
          then let (v1, v2) = v1v2 in
               (match rop with RDiff -> subst_AtomR_RDiff sign ra x y v1 v2
                 | RAdd -> subst_AtomR_RAdd sign ra x y v1 v2)
          else FactFm (AtomR (sign, r, x, y)))
    | r, rop, v1v2, Eq (sign, x, y) -> FactFm (Eq (sign, x, y));;

let rec push_isubst_concept _C
  x0 sbsts = match x0, sbsts with AtomC (sign, a), sbsts -> AtomC (sign, a)
    | Top, sbsts -> Top
    | Bottom, sbsts -> Bottom
    | NegC c, sbsts -> NegC (push_isubst_concept _C c sbsts)
    | BinopC (bop, c1, c2), sbsts ->
        BinopC
          (bop, push_isubst_concept _C c1 sbsts,
            push_isubst_concept _C c2 sbsts)
    | NumRestrC (nro, n, r, c), sbsts ->
        NumRestrC (nro, n, r, push_isubst_concept _C c sbsts)
    | SubstC (c, RSubst (r, rop, (x1, x2))), sbsts ->
        SubstC
          (push_isubst_concept _C c sbsts,
            RSubst (r, rop, (subst_vars _C sbsts x1, subst_vars _C sbsts x2)))
    | SubstC (c, CSubst (cr, rop, v)), sbsts ->
        SubstC
          (push_isubst_concept _C c sbsts,
            CSubst (cr, rop, subst_vars _C sbsts v))
    | SubstC (c, ISubst (x1, x2)), sbsts ->
        push_isubst_concept _C c ((x1, x2) :: sbsts)
    | SomeC (r, c), sbsts -> SomeC (r, push_isubst_concept _C c sbsts)
    | AllC (r, c), sbsts -> AllC (r, push_isubst_concept _C c sbsts);;

let rec push_isubst_fact _C
  x0 sbsts = match x0, sbsts with
    Inst (x, c), sbsts ->
      Inst (subst_vars _C sbsts x, push_isubst_concept _C c sbsts)
    | AtomR (sign, r, x, y), sbsts ->
        AtomR (sign, r, subst_vars _C sbsts x, subst_vars _C sbsts y)
    | Eq (sign, x, y), sbsts ->
        Eq (sign, subst_vars _C sbsts x, subst_vars _C sbsts y);;

let rec push_csubst_concept _A
  cr rop v x xa4 = match cr, rop, v, x, xa4 with
    cr, rop, v, x, AtomC (sign, a) ->
      (if eq _A cr a
        then (match rop
               with RDiff ->
                 neg_norm_form sign
                   (BinopFm
                     (Conj, FactFm (Inst (x, AtomC (true, a))),
                       FactFm (Eq (false, x, v))))
               | RAdd ->
                 neg_norm_form sign
                   (BinopFm
                     (Disj, FactFm (Inst (x, AtomC (true, a))),
                       FactFm (Eq (true, x, v)))))
        else FactFm (Inst (x, AtomC (sign, a))))
    | cr, rop, v, x, Top -> FactFm (Inst (x, Top))
    | cr, rop, v, x, Bottom -> FactFm (Inst (x, Bottom))
    | cr, rop, v, x, NegC c ->
        FactFm (Inst (x, NegC (SubstC (c, CSubst (cr, rop, v)))))
    | cr, rop, v, x, BinopC (bop, c1, c2) ->
        FactFm
          (Inst (x, BinopC
                      (bop, SubstC (c1, CSubst (cr, rop, v)),
                        SubstC (c2, CSubst (cr, rop, v)))))
    | cr, rop, v, x, NumRestrC (nro, n, r, c) ->
        FactFm
          (Inst (x, NumRestrC (nro, n, r, SubstC (c, CSubst (cr, rop, v)))))
    | cr, rop, v, x, SubstC (c, sb) ->
        SubstFm (FactFm (Inst (x, SubstC (c, sb))), CSubst (cr, rop, v))
    | cr, rop, v, x, SomeC (r, c) ->
        FactFm (Inst (x, SomeC (r, SubstC (c, CSubst (cr, rop, v)))))
    | cr, rop, v, x, AllC (r, c) ->
        FactFm (Inst (x, AllC (r, SubstC (c, CSubst (cr, rop, v)))));;

let rec push_csubst_fact _A
  ca rop v x3 = match ca, rop, v, x3 with
    ca, rop, v, Inst (x, c) -> push_csubst_concept _A ca rop v x c
    | c, rop, v, AtomR (sign, r, x, y) -> FactFm (AtomR (sign, r, x, y))
    | c, rop, v, Eq (sign, x, y) -> FactFm (Eq (sign, x, y));;

let rec push_subst_fact _A _B _C
  fct x1 = match fct, x1 with
    fct, RSubst (r, rop, p) -> push_rsubst_fact _A r rop p fct
    | fct, CSubst (c, rop, v) -> push_csubst_fact _B c rop v fct
    | fct, ISubst (v1, v2) -> FactFm (push_isubst_fact _C fct [(v1, v2)]);;

let rec push_subst_form _A _B _C
  x0 sbsts = match x0, sbsts with ConstFm cn, sbsts -> ConstFm cn
    | FactFm fct, sbsts ->
        (match extract_subst fct
          with None ->
            (match sbsts with [] -> FactFm fct
              | sb :: a ->
                push_subst_form _A _B _C (push_subst_fact _A _B _C fct sb) a)
          | Some (x, (c, RSubst (r, rop, v1v2))) ->
            push_subst_form _A _B _C (FactFm (Inst (x, c)))
              (RSubst (r, rop, v1v2) :: sbsts)
          | Some (x, (c, CSubst (cr, rop, v))) ->
            push_subst_form _A _B _C (FactFm (Inst (x, c)))
              (CSubst (cr, rop, v) :: sbsts)
          | Some (x, (c, ISubst (v1, v2))) ->
            push_subst_form _A _B _C
              (FactFm (Inst (x, push_isubst_concept _C c [(v1, v2)]))) sbsts)
    | NegFm f, sbsts -> NegFm (push_subst_form _A _B _C f sbsts)
    | BinopFm (bop, f1, f2), sbsts ->
        BinopFm
          (bop, push_subst_form _A _B _C f1 sbsts,
            push_subst_form _A _B _C f2 sbsts)
    | SubstFm (f, sb), sbsts -> push_subst_form _A _B _C f (sb :: sbsts);;

let rec push_isubst_subst _C
  x0 sbsts = match x0, sbsts with
    RSubst (r, rop, (x1, x2)), sbsts ->
      RSubst (r, rop, (subst_vars _C sbsts x1, subst_vars _C sbsts x2))
    | CSubst (cr, rop, v), sbsts -> CSubst (cr, rop, subst_vars _C sbsts v)
    | ISubst (v1, v2), sbsts ->
        ISubst (subst_vars _C sbsts v1, subst_vars _C sbsts v2);;

let rec push_isubst_form _C
  x0 sbsts = match x0, sbsts with ConstFm cn, sbsts -> ConstFm cn
    | FactFm fct, sbsts -> FactFm (push_isubst_fact _C fct sbsts)
    | NegFm f, sbsts -> NegFm (push_isubst_form _C f sbsts)
    | BinopFm (bop, f1, f2), sbsts ->
        BinopFm
          (bop, push_isubst_form _C f1 sbsts, push_isubst_form _C f2 sbsts)
    | SubstFm (f, sb), sbsts ->
        (match sb
          with RSubst (_, _, _) ->
            SubstFm (push_isubst_form _C f sbsts, push_isubst_subst _C sb sbsts)
          | CSubst (_, _, _) ->
            SubstFm (push_isubst_form _C f sbsts, push_isubst_subst _C sb sbsts)
          | ISubst (v1, v2) -> push_isubst_form _C f ((v1, v2) :: sbsts));;

let rec isubst_dom_list
  = function RSubst (r, rop, (v1, v2)) -> []
    | CSubst (c, rop, v) -> []
    | ISubst (va, v) -> [va];;

let rec fv_subst_list _C
  = function RSubst (r, rop, (v1, v2)) -> union _C [v1] [v2]
    | CSubst (c, rop, v) -> [v]
    | ISubst (va, v) -> [v];;

let rec fv_concept_list _C
  = function Bottom -> []
    | Top -> []
    | AtomC (sign, a) -> []
    | BinopC (bop, c1, c2) ->
        union _C (fv_concept_list _C c1) (fv_concept_list _C c2)
    | NegC c -> fv_concept_list _C c
    | NumRestrC (nro, n, r, c) -> fv_concept_list _C c
    | SubstC (c, sb) ->
        union _C (list_remove _C (fv_concept_list _C c) (isubst_dom_list sb))
          (fv_subst_list _C sb)
    | SomeC (r, c) -> fv_concept_list _C c
    | AllC (r, c) -> fv_concept_list _C c;;

let rec fv_fact_list _C
  = function Inst (x, c) -> insert _C x (fv_concept_list _C c)
    | AtomR (sign, r, x, y) -> [x; y]
    | Eq (sign, x, y) -> [x; y];;

let rec fv_form_list _C
  = function ConstFm cn -> []
    | FactFm fct -> fv_fact_list _C fct
    | NegFm f -> fv_form_list _C f
    | BinopFm (bop, f1, f2) ->
        list_union _C (fv_form_list _C f1) (fv_form_list _C f2)
    | SubstFm (f, sb) ->
        list_union _C (list_remove _C (fv_form_list _C f) (isubst_dom_list sb))
          (fv_subst_list _C sb);;

let rec name_of_free (Free v) = v;;

let rec lift_bound_substs
  x = map (fun (xa, y) -> (lift_var zero_nat xa, lift_var zero_nat y)) x;;

let rec push_isubst_qform _C
  x0 sbsts = match x0, sbsts with QConstFm cn, sbsts -> QConstFm cn
    | QFactFm fct, sbsts -> QFactFm (push_isubst_fact (equal_var _C) fct sbsts)
    | QNegFm f, sbsts -> QNegFm (push_isubst_qform _C f sbsts)
    | QBinopFm (bop, f1, f2), sbsts ->
        QBinopFm
          (bop, push_isubst_qform _C f1 sbsts, push_isubst_qform _C f2 sbsts)
    | QSubstFm (f, sb), sbsts ->
        (match sb
          with RSubst (_, _, _) ->
            QSubstFm
              (push_isubst_qform _C f sbsts,
                push_isubst_subst (equal_var _C) sb sbsts)
          | CSubst (_, _, _) ->
            QSubstFm
              (push_isubst_qform _C f sbsts,
                push_isubst_subst (equal_var _C) sb sbsts)
          | ISubst (v1, v2) -> push_isubst_qform _C f ((v1, v2) :: sbsts))
    | QuantifFm (q, v, f), sbsts ->
        QuantifFm (q, v, push_isubst_qform _C f (lift_bound_substs sbsts));;

let rec close_quantif
  = function Free v -> Free v
    | Bound n -> Bound (minus_nat n one_nat);;

let rec form_of_qform
  = function QConstFm cn -> ConstFm cn
    | QFactFm fct -> FactFm (apply_var_fact name_of_free fct)
    | QNegFm f -> NegFm (form_of_qform f)
    | QBinopFm (bop, f1, f2) ->
        BinopFm (bop, form_of_qform f1, form_of_qform f2)
    | QSubstFm (f, sb) ->
        SubstFm (form_of_qform f, apply_var_subst name_of_free sb);;

let rec fv_qform_list _C
  = function QConstFm cn -> []
    | QFactFm fct -> fv_fact_list (equal_var _C) fct
    | QNegFm f -> fv_qform_list _C f
    | QBinopFm (bop, f1, f2) ->
        list_union (equal_var _C) (fv_qform_list _C f1) (fv_qform_list _C f2)
    | QuantifFm (q, v, f) ->
        map close_quantif
          (list_remove (equal_var _C) (fv_qform_list _C f) [Bound zero_nat])
    | QSubstFm (f, sb) ->
        list_union (equal_var _C)
          (list_remove (equal_var _C) (fv_qform_list _C f) (isubst_dom_list sb))
          (fv_subst_list (equal_var _C) sb);;

let rec powerset
  = function [] -> [[]]
    | x :: xs -> let p = powerset xs in
                 map (fun a -> x :: a) p @ p;;

let rec size_list x = gen_length zero_nat x;;

let rec nnms_to_substs
  nnms =
    map (fun (n, vn) -> (Bound n, Free vn))
      (zip (upt zero_nat (size_list nnms)) nnms);;

let rec all_forms
  br = let Branch
             (n, (alf, (asf, (ap, Inactive_form
                                    (ico, (iac, (iar, (ie, icl))))))))
         = br in
       n @ alf @ asf @ ap @ ico @ iac @ iar @ ie @ icl;;

let rec fv_branch _C
  br = let a = all_forms br in
       foldl (fun res vs -> fv_form_list _C vs @ res) [] a;;

let rec n_subsets
  n xs = filter (fun ys -> equal_nat (size_list ys) n) (powerset xs);;

let rec is_falsefm
  f = (match f with ConstFm true -> false | ConstFm false -> true
        | FactFm _ -> false | NegFm _ -> false | BinopFm (_, _, _) -> false
        | SubstFm (_, _) -> false);;

let rec unequal_in _A _B _C
  br x1 x2 =
    let Branch (_, (_, (_, (_, Inactive_form (_, (_, (_, (ie, _)))))))) = br in
    member (equal_form _A _B _C) ie (FactFm (Eq (false, x1, x2))) ||
      member (equal_form _A _B _C) ie (FactFm (Eq (false, x2, x1)));;

let rec outgoing_rel_from _A _C
  br x r =
    let Branch (_, (_, (_, (_, Inactive_form (_, (_, (iar, (_, _)))))))) = br in
    remdups _C
      (foldl
        (fun res a ->
          (match a with ConstFm _ -> res | FactFm (Inst (_, _)) -> res
            | FactFm (AtomR (sign, ra, xa, y)) ->
              (if sign && (eq _C x xa && eq _A r ra) then y :: res else res)
            | FactFm (Eq (_, _, _)) -> res | NegFm _ -> res
            | BinopFm (_, _, _) -> res | SubstFm (_, _) -> res))
        [] iar);;

let rec set_of_pairs _A
  xs = maps (fun x1 ->
              maps (fun x2 -> (if less _A x1 x2 then [(x1, x2)] else [])) xs)
         xs;;

let rec having_class _A _B _C
  br x c =
    let Branch (_, (alf, (asf, (_, Inactive_form (ico, (iac, (_, (_, _)))))))) =
      br in
    list_ex
      (fun a ->
        (match a with ConstFm _ -> false
          | FactFm (Inst (xa, ca)) -> eq _C x xa && equal_concept _A _B _C c ca
          | FactFm (AtomR (_, _, _, _)) -> false
          | FactFm (Eq (_, _, _)) -> false | NegFm _ -> false
          | BinopFm (_, _, _) -> false | SubstFm (_, _) -> false))
      (alf @ asf @ ico @ iac);;

let rec numrestrc_lt_applicable_vars _A _B (_C1, _C2)
  br x n r c =
    let ys_outgoing = remdups _C1 (outgoing_rel_from _A _C1 br x r) in
    let ys_c = filter (fun y -> having_class _A _B _C1 br y c) ys_outgoing in
    (if less_nat (size_list ys_c) n then []
      else let a = set_of_pairs _C2 ys_c in
           filter (fun (x1, x2) -> not (unequal_in _A _B _C1 br x1 x2)) a);;

let rec numrestrc_lt_applicable_branch _A _B (_C1, _C2)
  br x n r c =
    not (null (numrestrc_lt_applicable_vars _A _B (_C1, _C2) br x n r c));;

let rec choose_applicable_vars _A _B _C
  br x r c =
    let ys_outgoing = remdups _C (outgoing_rel_from _A _C br x r) in
    let ys_c = filter (fun y -> having_class _A _B _C br y c) ys_outgoing in
    let not_c = neg_norm_concept false c in
    let ys_not_c =
      filter (fun y -> having_class _A _B _C br y not_c) ys_outgoing in
    let ys_undet =
      filter (fun y -> not (member _C ys_c y) && not (member _C ys_not_c y))
        ys_outgoing
      in
    let a = list_inters _C ys_c ys_not_c in
    (ys_undet, a);;

let rec choose_applicable_in_branch _A _B _C
  br x r c = not (null (fst (choose_applicable_vars _A _B _C br x r c)));;

let rec is_applicable_permanent _A _B (_C1, _C2)
  br f =
    (match f with ConstFm _ -> false | FactFm (Inst (x, Top)) -> false
      | FactFm (Inst (x, Bottom)) -> false
      | FactFm (Inst (x, AtomC (_, _))) -> false
      | FactFm (Inst (x, NegC _)) -> false
      | FactFm (Inst (x, BinopC (_, _, _))) -> false
      | FactFm (Inst (x, NumRestrC (Lt, n, r, c))) ->
        choose_applicable_in_branch _A _B _C1 br x r c ||
          numrestrc_lt_applicable_branch _A _B (_C1, _C2) br x n r c
      | FactFm (Inst (x, NumRestrC (Ge, n, r, c))) -> false
      | FactFm (Inst (x, SubstC (_, _))) -> false
      | FactFm (Inst (x, SomeC (_, _))) -> false
      | FactFm (Inst (x, AllC (_, _))) -> false
      | FactFm (AtomR (_, _, _, _)) -> false | FactFm (Eq (_, _, _)) -> false
      | NegFm _ -> false | BinopFm (_, _, _) -> false
      | SubstFm (_, _) -> false);;

let rec is_inactive _A _B (_C1, _C2)
  br = (match br
         with Branch ([], ([], ([], (ap, _)))) ->
           pred_list
             (fun f -> not (is_applicable_permanent _A _B (_C1, _C2) br f)) ap
         | Branch ([], ([], (_ :: _, b))) -> false
         | Branch ([], (_ :: _, ba)) -> false | Branch (_ :: _, b) -> false);;

let rec is_neg_role _A _B _C
  f fn =
    (match fn with ConstFm _ -> false | FactFm (Inst (_, _)) -> false
      | FactFm (AtomR (sign, r, x, y)) ->
        equal_forma _A _B _C f (FactFm (AtomR (not sign, r, x, y)))
      | FactFm (Eq (_, _, _)) -> false | NegFm _ -> false
      | BinopFm (_, _, _) -> false | SubstFm (_, _) -> false);;

let rec add_new_form
  f br =
    let Branch (n, (alf, (asf, (ap, ia)))) = br in
    Branch (f :: n, (alf, (asf, (ap, ia))));;

let rec mutually_distinct _A _B _C
  br xs =
    pred_list
      (fun x1 ->
        pred_list (fun x2 -> eq _C x1 x2 || unequal_in _A _B _C br x1 x2) xs)
      xs;;

let rec exist_outgoing_r_c_distincts_from _A _B _C
  br x n r c =
    let ys_outgoing = outgoing_rel_from _A _C br x r in
    let ys_c = filter (fun y -> having_class _A _B _C br y c) ys_outgoing in
    let a = n_subsets n ys_c in
    list_ex (mutually_distinct _A _B _C br) a;;

let rec numrestrc_ge_applicable_branch _A _B _C
  br x n r c = not (exist_outgoing_r_c_distincts_from _A _B _C br x n r c);;

let rec make_outgoing_r_c_from
  x r c nvrs =
    map (fun y -> FactFm (AtomR (true, r, x, y))) nvrs @
      map (fun y -> FactFm (Inst (y, c))) nvrs;;

let rec make_mutually_distinct _A
  nvrs = map (fun (x, y) -> FactFm (Eq (false, x, y))) (set_of_pairs _A nvrs);;

let rec new_vars_list _A
  n vns vn =
    (if equal_nat n zero_nat then []
      else let nvar = new_var_list _A vns vn in
           nvar :: new_vars_list _A (minus_nat n one_nat) (nvar :: vns) vn);;

let rec numrestrc_ge_generated (_C1, _C2, _C3)
  br x n r c =
    let vrs = fv_branch _C1 br in
    let nvrs = new_vars_list _C2 n vrs x in
    make_mutually_distinct _C3 nvrs @ make_outgoing_r_c_from x r c nvrs;;

let rec bag_insert_front _A x xs = (if member _A xs x then xs else x :: xs);;

let rec add_inactive_composite _A _B _C
  f br =
    let Branch
          (n, (alf, (asf, (ap, Inactive_form (ico, (iac, (iar, (ie, icl))))))))
      = br in
    Branch
      (n, (alf, (asf, (ap, Inactive_form
                             (bag_insert_front (equal_form _A _B _C) f ico,
                               (iac, (iar, (ie, icl))))))));;

let rec add_new_forms
  fs br =
    let Branch (n, (alf, (asf, (ap, ia)))) = br in
    Branch (fs @ n, (alf, (asf, (ap, ia))));;

let rec apply_numrestrc_ge_branch _A _B (_C1, _C2, _C3)
  br f x n r c =
    (if equal_nat n zero_nat
      then AppRes (zero_nat, None, [add_inactive_composite _A _B _C1 f br])
      else (if numrestrc_ge_applicable_branch _A _B _C1 br x n r c
             then AppRes
                    (one_nat, Some Todo_rep,
                      [add_new_forms
                         (numrestrc_ge_generated (_C1, _C2, _C3) br x n r c)
                         (add_inactive_composite _A _B _C1 f br)])
             else AppRes
                    (zero_nat, None,
                      [add_inactive_composite _A _B _C1 f br])));;

let rec propagate_equality _A _B _C
  x y xs =
    remdups (equal_form _B _C _A)
      (map (fun frm -> push_isubst_form _A frm [(x, y)]) xs);;

let rec subst_applicable_branch _A _B _C
  br fp = let fs = all_forms br in
          not (member (equal_form _A _B _C) fs fp);;

let rec apply_subst_branch _A _B _C
  br f =
    let fp = neg_norm_form true (push_subst_form _A _B _C f []) in
    (if subst_applicable_branch _A _B _C br fp
      then AppRes
             (one_nat, Some Todo_rep,
               [add_new_form fp (add_inactive_composite _A _B _C f br)])
      else AppRes (zero_nat, None, [add_inactive_composite _A _B _C f br]));;

let rec reactivate_equivs
  br = let Branch
             (n, (alf, (asf, (ap, Inactive_form
                                    (ico, (iac, (iar, (ie, icl))))))))
         = br in
       Branch
         (n @ ie,
           (alf, (asf, (ap, Inactive_form (ico, (iac, (iar, ([], icl))))))));;

let rec conj_applicable_branch _A _B _C
  br f1 f2 =
    let fs = all_forms br in
    not (member (equal_form _A _B _C) fs f1 &&
          member (equal_form _A _B _C) fs f2);;

let rec apply_conj_branch _A _B _C
  br f f1 f2 =
    (if conj_applicable_branch _A _B _C br f1 f2
      then AppRes
             (one_nat, Some (ConjRule_rep f),
               [add_new_forms [f2; f1] (add_inactive_composite _A _B _C f br)])
      else AppRes (zero_nat, None, [add_inactive_composite _A _B _C f br]));;

let rec and_applicable_branch _A _B _C
  br x c1 c2 =
    let fs = all_forms br in
    not (member (equal_form _A _B _C) fs (FactFm (Inst (x, c1))) &&
          member (equal_form _A _B _C) fs (FactFm (Inst (x, c2))));;

let rec apply_and_branch _A _B _C
  br f x c1 c2 =
    (if and_applicable_branch _A _B _C br x c1 c2
      then AppRes
             (one_nat, Some (AndRule_rep f),
               [add_new_forms [FactFm (Inst (x, c2)); FactFm (Inst (x, c1))]
                  (add_inactive_composite _A _B _C f br)])
      else AppRes (zero_nat, None, [add_inactive_composite _A _B _C f br]));;

let rec inactive_form_apply
  f iaf =
    let Inactive_form (ico, (iac, (iar, (ie, icl)))) = iaf in
    Inactive_form (f ico, (f iac, (f iar, (f ie, f icl))));;

let rec branch_apply
  f br =
    let Branch (n, (alf, (asf, (ap, ia)))) = br in
    Branch (f n, (f alf, (f asf, (f ap, inactive_form_apply f ia))));;

let rec apply_linear _A _B (_C1, _C2, _C3)
  br f =
    (match f
      with ConstFm _ ->
        AppRes (zero_nat, None, [add_inactive_composite _A _B _C1 f br])
      | FactFm (Inst (x, Top)) ->
        AppRes (zero_nat, None, [add_inactive_composite _A _B _C1 f br])
      | FactFm (Inst (x, Bottom)) ->
        AppRes (zero_nat, None, [add_inactive_composite _A _B _C1 f br])
      | FactFm (Inst (x, AtomC (_, _))) ->
        AppRes (zero_nat, None, [add_inactive_composite _A _B _C1 f br])
      | FactFm (Inst (x, NegC _)) ->
        AppRes (zero_nat, None, [add_inactive_composite _A _B _C1 f br])
      | FactFm (Inst (x, BinopC (Conj, c1, c2))) ->
        apply_and_branch _A _B _C1 br f x c1 c2
      | FactFm (Inst (x, BinopC (Disj, c1, c2))) ->
        AppRes (zero_nat, None, [add_inactive_composite _A _B _C1 f br])
      | FactFm (Inst (x, NumRestrC (Lt, n, r, c))) ->
        AppRes (zero_nat, None, [add_inactive_composite _A _B _C1 f br])
      | FactFm (Inst (x, NumRestrC (Ge, n, r, c))) ->
        apply_numrestrc_ge_branch _A _B (_C1, _C2, _C3) br f x n r c
      | FactFm (Inst (x, SubstC (_, _))) -> apply_subst_branch _A _B _C1 br f
      | FactFm (Inst (x, SomeC (_, _))) ->
        AppRes (zero_nat, None, [add_inactive_composite _A _B _C1 f br])
      | FactFm (Inst (x, AllC (_, _))) ->
        AppRes (zero_nat, None, [add_inactive_composite _A _B _C1 f br])
      | FactFm (AtomR (_, _, _, _)) ->
        AppRes (zero_nat, None, [add_inactive_composite _A _B _C1 f br])
      | FactFm (Eq (true, x, y)) ->
        AppRes
          (one_nat, Some Todo_rep,
            [reactivate_equivs
               (branch_apply (propagate_equality _C1 _A _B x y)
                 (add_inactive_composite _A _B _C1 f br))])
      | FactFm (Eq (false, x, y)) ->
        AppRes (zero_nat, None, [add_inactive_composite _A _B _C1 f br])
      | NegFm _ ->
        AppRes (zero_nat, None, [add_inactive_composite _A _B _C1 f br])
      | BinopFm (Conj, f1, f2) -> apply_conj_branch _A _B _C1 br f f1 f2
      | BinopFm (Disj, f1, f2) ->
        AppRes (zero_nat, None, [add_inactive_composite _A _B _C1 f br])
      | SubstFm (_, _) -> apply_subst_branch _A _B _C1 br f);;

let rec add_active_splitting _A _B _C
  f br =
    let Branch (n, (alf, (asf, (ap, ia)))) = br in
    Branch
      (n, (alf, (bag_insert_front (equal_form _A _B _C) f asf, (ap, ia))));;

let rec add_inactive_clash _A _B _C
  f br =
    let Branch
          (n, (alf, (asf, (ap, Inactive_form (ico, (iac, (iar, (ie, icl))))))))
      = br in
    Branch
      (n, (alf, (asf, (ap, Inactive_form
                             (ico, (iac, (iar,
   (ie, bag_insert_front (equal_form _A _B _C) f icl))))))));;

let rec add_active_linear _A _B _C
  f br =
    let Branch (n, (alf, (asf, (ap, ia)))) = br in
    Branch
      (n, (bag_insert_front (equal_form _A _B _C) f alf, (asf, (ap, ia))));;

let rec add_inactive_equivs _A _B _C
  f br =
    let Branch
          (n, (alf, (asf, (ap, Inactive_form (ico, (iac, (iar, (ie, icl))))))))
      = br in
    Branch
      (n, (alf, (asf, (ap, Inactive_form
                             (ico, (iac, (iar,
   (bag_insert_front (equal_form _A _B _C) f ie, icl))))))));;

let rec add_inactive_atomr _A _B _C
  f br =
    let Branch
          (n, (alf, (asf, (ap, Inactive_form (ico, (iac, (iar, (ie, icl))))))))
      = br in
    Branch
      (n, (alf, (asf, (ap, Inactive_form
                             (ico, (iac, (bag_insert_front (equal_form _A _B _C)
    f iar,
   (ie, icl))))))));;

let rec bag_insert_end _A x xs = (if member _A xs x then xs else xs @ [x]);;

let rec add_active_permanent _A _B _C
  f br =
    let Branch (n, (alf, (asf, (ap, ia)))) = br in
    Branch (n, (alf, (asf, (bag_insert_end (equal_form _A _B _C) f ap, ia))));;

let rec add_inactive_atomc _A _B _C
  f br =
    let Branch
          (n, (alf, (asf, (ap, Inactive_form (ico, (iac, (iar, (ie, icl))))))))
      = br in
    Branch
      (n, (alf, (asf, (ap, Inactive_form
                             (ico, (bag_insert_front (equal_form _A _B _C) f
                                      iac,
                                     (iar, (ie, icl))))))));;

let rec classify_concept _A _B _C
  br f =
    (match f with FactFm (Inst (_, Top)) -> add_inactive_composite _A _B _C f br
      | FactFm (Inst (_, Bottom)) -> add_inactive_clash _A _B _C f br
      | FactFm (Inst (_, AtomC (_, _))) -> add_inactive_atomc _A _B _C f br
      | FactFm (Inst (_, NegC _)) -> add_inactive_composite _A _B _C f br
      | FactFm (Inst (_, BinopC (Conj, _, _))) ->
        add_active_linear _A _B _C f br
      | FactFm (Inst (_, BinopC (Disj, _, _))) ->
        add_active_splitting _A _B _C f br
      | FactFm (Inst (_, NumRestrC (Lt, _, _, _))) ->
        add_active_permanent _A _B _C f br
      | FactFm (Inst (_, NumRestrC (Ge, _, _, _))) ->
        add_active_linear _A _B _C f br
      | FactFm (Inst (_, SubstC (_, _))) -> add_active_linear _A _B _C f br
      | FactFm (Inst (_, SomeC (_, _))) -> add_active_linear _A _B _C f br
      | FactFm (Inst (_, AllC (_, _))) -> add_active_linear _A _B _C f br);;

let rec classify_fact _A _B _C
  br f =
    (match f with FactFm (Inst (_, _)) -> classify_concept _A _B _C br f
      | FactFm (AtomR (_, _, _, _)) -> add_inactive_atomr _A _B _C f br
      | FactFm (Eq (true, x, y)) ->
        (if eq _C x y then add_inactive_composite _A _B _C f br
          else add_active_linear _A _B _C f br)
      | FactFm (Eq (false, x, y)) ->
        (if eq _C x y then add_inactive_clash _A _B _C f br
          else add_inactive_equivs _A _B _C f br));;

let rec classify_form _A _B _C
  br f =
    (match f with ConstFm true -> add_inactive_composite _A _B _C f br
      | ConstFm false -> add_inactive_clash _A _B _C f br
      | FactFm _ -> classify_fact _A _B _C br f
      | NegFm fa -> add_inactive_composite _A _B _C fa br
      | BinopFm (Conj, _, _) -> add_active_linear _A _B _C f br
      | BinopFm (Disj, _, _) -> add_active_splitting _A _B _C f br
      | SubstFm (_, _) -> add_active_linear _A _B _C f br);;

let rec classify_new _A _B _C
  br = (match br with Branch ([], (_, (_, (_, _)))) -> br
         | Branch (nf :: nfs, (alf, (asf, (ap, ia)))) ->
           classify_form _A _B _C (Branch (nfs, (alf, (asf, (ap, ia))))) nf);;

let rec is_ineq_inst _C
  f = (match f with ConstFm _ -> false | FactFm (Inst (_, _)) -> false
        | FactFm (AtomR (_, _, _, _)) -> false
        | FactFm (Eq (true, x, y)) -> false
        | FactFm (Eq (false, x, y)) -> eq _C x y | NegFm _ -> false
        | BinopFm (_, _, _) -> false | SubstFm (_, _) -> false);;

let rec map_classify_branches _A _B _C
  ar = let AppRes (n, rr_opt, brs) = ar in
       AppRes (n, rr_opt, map (classify_new _A _B _C) brs);;

let rec disj_applicable_branch _A _B _C
  br f1 f2 =
    let fs = all_forms br in
    not (member (equal_form _A _B _C) fs f1) &&
      not (member (equal_form _A _B _C) fs f2);;

let rec apply_disj_branch _A _B _C
  br f f1 f2 =
    (if disj_applicable_branch _A _B _C br f1 f2
      then AppRes
             (nat_of_integer (Big_int.big_int_of_int 2), Some (DisjRule_rep f),
               [add_new_form f1 (add_inactive_composite _A _B _C f br);
                 add_new_form f2 (add_inactive_composite _A _B _C f br)])
      else AppRes (zero_nat, None, [add_inactive_composite _A _B _C f br]));;

let rec or_applicable_branch _A _B _C
  br x c1 c2 =
    let fs = all_forms br in
    not (member (equal_form _A _B _C) fs (FactFm (Inst (x, c1)))) &&
      not (member (equal_form _A _B _C) fs (FactFm (Inst (x, c2))));;

let rec apply_or_branch _A _B _C
  br f x c1 c2 =
    (if or_applicable_branch _A _B _C br x c1 c2
      then AppRes
             (nat_of_integer (Big_int.big_int_of_int 2), Some (OrRule_rep f),
               [add_new_form (FactFm (Inst (x, c1)))
                  (add_inactive_composite _A _B _C f br);
                 add_new_form (FactFm (Inst (x, c2)))
                   (add_inactive_composite _A _B _C f br)])
      else AppRes (zero_nat, None, [add_inactive_composite _A _B _C f br]));;

let rec apply_splitting _A _B _C
  br f =
    (match f
      with ConstFm _ ->
        AppRes (zero_nat, None, [add_inactive_composite _A _B _C f br])
      | FactFm (Inst (x, Top)) ->
        AppRes (zero_nat, None, [add_inactive_composite _A _B _C f br])
      | FactFm (Inst (x, Bottom)) ->
        AppRes (zero_nat, None, [add_inactive_composite _A _B _C f br])
      | FactFm (Inst (x, AtomC (_, _))) ->
        AppRes (zero_nat, None, [add_inactive_composite _A _B _C f br])
      | FactFm (Inst (x, NegC _)) ->
        AppRes (zero_nat, None, [add_inactive_composite _A _B _C f br])
      | FactFm (Inst (x, BinopC (Conj, c1, c2))) ->
        AppRes (zero_nat, None, [add_inactive_composite _A _B _C f br])
      | FactFm (Inst (x, BinopC (Disj, c1, c2))) ->
        apply_or_branch _A _B _C br f x c1 c2
      | FactFm (Inst (x, NumRestrC (_, _, _, _))) ->
        AppRes (zero_nat, None, [add_inactive_composite _A _B _C f br])
      | FactFm (Inst (x, SubstC (_, _))) -> apply_subst_branch _A _B _C br f
      | FactFm (Inst (x, SomeC (_, _))) ->
        AppRes (zero_nat, None, [add_inactive_composite _A _B _C f br])
      | FactFm (Inst (x, AllC (_, _))) ->
        AppRes (zero_nat, None, [add_inactive_composite _A _B _C f br])
      | FactFm (AtomR (_, _, _, _)) ->
        AppRes (zero_nat, None, [add_inactive_composite _A _B _C f br])
      | FactFm (Eq (_, _, _)) ->
        AppRes (zero_nat, None, [add_inactive_composite _A _B _C f br])
      | NegFm _ ->
        AppRes (zero_nat, None, [add_inactive_composite _A _B _C f br])
      | BinopFm (Conj, f1, f2) ->
        AppRes (zero_nat, None, [add_inactive_composite _A _B _C f br])
      | BinopFm (Disj, f1, f2) -> apply_disj_branch _A _B _C br f f1 f2
      | SubstFm (_, _) ->
        AppRes (zero_nat, None, [add_inactive_composite _A _B _C f br]));;

let rec apply_permanent _A _B (_C1, _C2)
  br f =
    (match f
      with ConstFm _ ->
        AppRes (zero_nat, None, [add_inactive_composite _A _B _C1 f br])
      | FactFm (Inst (x, Top)) ->
        AppRes (zero_nat, None, [add_inactive_composite _A _B _C1 f br])
      | FactFm (Inst (x, Bottom)) ->
        AppRes (zero_nat, None, [add_inactive_composite _A _B _C1 f br])
      | FactFm (Inst (x, AtomC (_, _))) ->
        AppRes (zero_nat, None, [add_inactive_composite _A _B _C1 f br])
      | FactFm (Inst (x, NegC _)) ->
        AppRes (zero_nat, None, [add_inactive_composite _A _B _C1 f br])
      | FactFm (Inst (x, BinopC (_, _, _))) ->
        AppRes (zero_nat, None, [add_inactive_composite _A _B _C1 f br])
      | FactFm (Inst (x, NumRestrC (Lt, n, r, c))) ->
        (match numrestrc_lt_applicable_vars _A _B (_C1, _C2) br x n r c
          with [] ->
            (match choose_applicable_vars _A _B _C1 br x r c
              with ([], []) ->
                AppRes (zero_nat, None, [add_active_permanent _A _B _C1 f br])
              | ([], _ :: _) ->
                AppRes
                  (one_nat, Some Todo_rep,
                    [add_inactive_clash _A _B _C1 (ConstFm false) br])
              | (y :: _, xa) ->
                AppRes
                  (nat_of_integer (Big_int.big_int_of_int 2), Some Todo_rep,
                    [add_new_form (FactFm (Inst (y, c)))
                       (add_active_permanent _A _B _C1 f br);
                      add_new_form (FactFm (Inst (y, neg_norm_concept false c)))
                        (add_active_permanent _A _B _C1 f br)]))
          | a :: lista ->
            AppRes
              (size_list (a :: lista), Some Todo_rep,
                map (fun (xa, y) ->
                      add_new_form (FactFm (Eq (true, xa, y)))
                        (add_active_permanent _A _B _C1 f br))
                  (a :: lista)))
      | FactFm (Inst (x, NumRestrC (Ge, n, r, c))) ->
        AppRes (zero_nat, None, [add_inactive_composite _A _B _C1 f br])
      | FactFm (Inst (x, SubstC (_, _))) ->
        AppRes (zero_nat, None, [add_inactive_composite _A _B _C1 f br])
      | FactFm (Inst (x, SomeC (_, _))) ->
        AppRes (zero_nat, None, [add_inactive_composite _A _B _C1 f br])
      | FactFm (Inst (x, AllC (_, _))) ->
        AppRes (zero_nat, None, [add_inactive_composite _A _B _C1 f br])
      | FactFm (AtomR (_, _, _, _)) ->
        AppRes (zero_nat, None, [add_inactive_composite _A _B _C1 f br])
      | FactFm (Eq (_, _, _)) ->
        AppRes (zero_nat, None, [add_inactive_composite _A _B _C1 f br])
      | NegFm _ ->
        AppRes (zero_nat, None, [add_inactive_composite _A _B _C1 f br])
      | BinopFm (_, _, _) ->
        AppRes (zero_nat, None, [add_inactive_composite _A _B _C1 f br])
      | SubstFm (_, _) ->
        AppRes (zero_nat, None, [add_inactive_composite _A _B _C1 f br]));;

let rec apply_rules_pt _A _B (_C1, _C2, _C3)
  = function
    Branch ([], ([], ([], ([], ia)))) ->
      AppRes (zero_nat, None, [Branch ([], ([], ([], ([], ia))))])
    | Branch ([], ([], ([], (apf :: apfs, ia)))) ->
        map_classify_branches _A _B _C1
          (apply_permanent _A _B (_C1, _C3)
            (Branch ([], ([], ([], (apfs, ia))))) apf)
    | Branch ([], ([], (asf :: asfs, (ap, ia)))) ->
        map_classify_branches _A _B _C1
          (apply_splitting _A _B _C1 (Branch ([], ([], (asfs, (ap, ia))))) asf)
    | Branch ([], (alf :: alfs, (asf, (ap, ia)))) ->
        map_classify_branches _A _B _C1
          (apply_linear _A _B (_C1, _C2, _C3)
            (Branch ([], (alfs, (asf, (ap, ia))))) alf)
    | Branch (vc :: vd, vb) ->
        AppRes
          (zero_nat, None, [classify_new _A _B _C1 (Branch (vc :: vd, vb))]);;

let rec is_neg_concept _A _B _C
  f fn =
    (match fn with ConstFm _ -> false | FactFm (Inst (x, Top)) -> false
      | FactFm (Inst (x, Bottom)) -> false
      | FactFm (Inst (x, AtomC (sign, a))) ->
        equal_forma _A _B _C f (FactFm (Inst (x, AtomC (not sign, a))))
      | FactFm (Inst (x, NegC _)) -> false
      | FactFm (Inst (x, BinopC (_, _, _))) -> false
      | FactFm (Inst (x, NumRestrC (_, _, _, _))) -> false
      | FactFm (Inst (x, SubstC (_, _))) -> false
      | FactFm (Inst (x, SomeC (_, _))) -> false
      | FactFm (Inst (x, AllC (_, _))) -> false
      | FactFm (AtomR (_, _, _, _)) -> false | FactFm (Eq (_, _, _)) -> false
      | NegFm _ -> false | BinopFm (_, _, _) -> false
      | SubstFm (_, _) -> false);;

let rec free_univ_bound_list (_C1, _C2)
  x0 nnms fvs = match x0, nnms, fvs with
    QuantifFm (QAll, v, f), nnms, fvs ->
      let nv_name = new_var_list _C2 (nnms @ fvs) v in
      free_univ_bound_list (_C1, _C2) f (nv_name :: nnms) fvs
    | QConstFm v, nnms, fvs ->
        push_isubst_qform _C1 (QConstFm v) (nnms_to_substs nnms)
    | QFactFm v, nnms, fvs ->
        push_isubst_qform _C1 (QFactFm v) (nnms_to_substs nnms)
    | QNegFm v, nnms, fvs ->
        push_isubst_qform _C1 (QNegFm v) (nnms_to_substs nnms)
    | QBinopFm (v, va, vb), nnms, fvs ->
        push_isubst_qform _C1 (QBinopFm (v, va, vb)) (nnms_to_substs nnms)
    | QuantifFm (QEx, va, vb), nnms, fvs ->
        push_isubst_qform _C1 (QuantifFm (QEx, va, vb)) (nnms_to_substs nnms)
    | QSubstFm (v, va), nnms, fvs ->
        push_isubst_qform _C1 (QSubstFm (v, va)) (nnms_to_substs nnms);;

let rec free_prenex_form_list (_C1, _C2)
  f = form_of_qform
        (free_univ_bound_list (_C1, _C2) (lift_bound f) []
          (map name_of_free (fv_qform_list _C1 f)));;

let rec contains_numrestr_clash_branch _A _B _C
  br = let Branch (_, (_, (_, (ap, _)))) = br in
       list_ex
         (fun a ->
           (match a with ConstFm _ -> false | FactFm (Inst (x, Top)) -> false
             | FactFm (Inst (x, Bottom)) -> false
             | FactFm (Inst (x, AtomC (_, _))) -> false
             | FactFm (Inst (x, NegC _)) -> false
             | FactFm (Inst (x, BinopC (_, _, _))) -> false
             | FactFm (Inst (x, NumRestrC (Lt, n, r, c))) ->
               (if equal_nat n zero_nat then true
                 else exist_outgoing_r_c_distincts_from _A _B _C br x n r c)
             | FactFm (Inst (x, NumRestrC (Ge, n, r, c))) -> false
             | FactFm (Inst (x, SubstC (_, _))) -> false
             | FactFm (Inst (x, SomeC (_, _))) -> false
             | FactFm (Inst (x, AllC (_, _))) -> false
             | FactFm (AtomR (_, _, _, _)) -> false
             | FactFm (Eq (_, _, _)) -> false | NegFm _ -> false
             | BinopFm (_, _, _) -> false | SubstFm (_, _) -> false))
         ap;;

let rec contains_contr_concept_branch _A _B _C
  br = let Branch (_, (_, (_, (_, Inactive_form (_, (iac, (_, (_, _)))))))) = br
         in
       list_ex (fun f -> list_ex (is_neg_concept _A _B _C f) iac) iac;;

let rec contains_contr_role_branch _A _B _C
  br = let Branch (_, (_, (_, (_, Inactive_form (_, (_, (iar, (_, _)))))))) = br
         in
       list_ex (fun f -> list_ex (is_neg_role _A _B _C f) iar) iar;;

let rec nonempty_clashset_branch
  br = let Branch (_, (_, (_, (_, Inactive_form (_, (_, (_, (_, icl)))))))) = br
         in
       not (null icl);;

let rec contains_contr_eq_branch _C
  br = let Branch (_, (_, (_, (_, Inactive_form (_, (_, (_, (_, a)))))))) = br
         in
       list_ex (is_ineq_inst _C) a;;

let rec contains_falsefm_branch
  br = let Branch (_, (_, (_, (_, Inactive_form (_, (_, (_, (_, a)))))))) = br
         in
       list_ex is_falsefm a;;

let rec contains_clash_branch _A _B _C
  br = (match br
         with Branch ([], (_, (_, (_, _)))) ->
           nonempty_clashset_branch br ||
             (contains_contr_concept_branch _A _B _C br ||
               (contains_contr_role_branch _A _B _C br ||
                 (contains_contr_eq_branch _C br ||
                   (contains_falsefm_branch br ||
                     contains_numrestr_clash_branch _A _B _C br))))
         | Branch (_ :: _, b) -> false);;

let rec free_prenex_form_list_string
  x = free_prenex_form_list (equal_literal, new_var_list_class_literal) x;;

end;; (*struct VerifCondition*)
