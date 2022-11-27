%{

open Verif_condition.VerifCondition ;;
open Lang ;;

let parse_error p = (* Called by the parser function on error *)
  print_endline p;
  flush stdout
;;

%}

%token <string> IDENTIFIER
%token <int> INTCONSTANT
%token <string> STRINGCONSTANT
%token <string> CHARCONSTANT
%token LPAREN RPAREN
%token DOT
%token NEW CLASS THIS NULL VOID
%token LSQBRACE RSQBRACE
%token EXCLMARK
%token PLUS MINUS TIMES DIV MOD
%token LBRACE RBRACE
%token EQ NEQ COMMA SEMICOLON COLON QMARK
%token IF ELSE WHILE FOR RETURN BCEQ BCGE BCGT BCLE BCLT BCNE BLAND BLOR SYNCHRONIZED DO
%token EOF

/* New */
%token PRE POST INV
%token VARS CONCEPTS ROLES
%token SELECT WITH DELETE ADD
%token TRUE FALSE
%token CONCTOP CONCBOT CONCALL CONCEX CONCAND CONCOR
/* End New */

%right ELSE
%right CATCH

%start start
%type <(string, string, string) Lang.prog> start

%%

start: prog { $1 }
;

prog:
  allDecls precond block_item_list_opt postcond EOF
  {Prog($1, $2, $3, $4)}
;

allDecls: 
  varDecls conceptDecls roleDecls  
  {Decls($1, $2, $3)}
;

varDecls:  
  VARS idListCommaSep SEMICOLON 
  {$2}
;

conceptDecls:  
  CONCEPTS idListCommaSep SEMICOLON 
  {$2}
;

roleDecls:  
  ROLES idListCommaSep SEMICOLON 
  {$2}
;

precond:
  PRE COLON form SEMICOLON
  {$3}
;

postcond:
  POST COLON form SEMICOLON
  {$3}
;

invariant:
  INV COLON form SEMICOLON
  {$3}
;

idListCommaSep:
  IDENTIFIER
{[$1]}
| IDENTIFIER COMMA idListCommaSep
{[$1]@$3}
;


/* *******  FORMULAS  ******* */


primary_concept:
  CONCTOP
    { Top }
| CONCBOT
    { Bottom }
| IDENTIFIER
  { AtomC (true,$1) }
| LPAREN concept RPAREN 
  { $2 }
| LPAREN CONCALL IDENTIFIER concept RPAREN 
/*  { AllC ($3, $4) } */
/* Disabled until prover can treat NumRestrC */
  { NumRestrC (Lt, Nat (Big_int.big_int_of_int 1), $3, (NegC $4)) }

| LPAREN CONCEX IDENTIFIER concept RPAREN 
/*  { SomeC ($3, $4) } */
  { NumRestrC (Ge, Nat (Big_int.big_int_of_int 1), $3, $4) }
| LPAREN BCGE INTCONSTANT IDENTIFIER concept RPAREN 
  { NumRestrC (Ge, Nat (Big_int.big_int_of_int $3), $4, $5) }
| LPAREN BCGT INTCONSTANT IDENTIFIER concept RPAREN 
  { NumRestrC (Ge, Nat (Big_int.big_int_of_int ($3 + 1)), $4, $5) }
| LPAREN BCLE INTCONSTANT IDENTIFIER concept RPAREN 
  { NumRestrC (Lt, Nat (Big_int.big_int_of_int ($3 + 1)), $4, $5) }
| LPAREN BCLT INTCONSTANT IDENTIFIER concept RPAREN 
  { NumRestrC (Lt, Nat (Big_int.big_int_of_int $3), $4, $5) }
| LPAREN EQ INTCONSTANT IDENTIFIER concept RPAREN
  { andC(NumRestrC (Ge, Nat (Big_int.big_int_of_int $3), $4, $5),
         NumRestrC (Lt, Nat (Big_int.big_int_of_int ($3 + 1)), $4, $5)) }
;


unary_concept:
  primary_concept
    { $1 }
| EXCLMARK primary_concept
    { NegC($2) }
;

and_concept:
    unary_concept
    { $1 }
| unary_concept CONCAND and_concept
    { andC($1, $3) }
;

or_concept: 
  and_concept
    { $1 }
| and_concept CONCOR or_concept
    { orC($1, $3) }
;

concept:
  or_concept
    { $1 }
;

fact: 
  IDENTIFIER COLON concept
  { Inst($1, $3) }
| LPAREN IDENTIFIER IDENTIFIER IDENTIFIER RPAREN 
  { AtomR(true, $3, $2, $4) }
| LPAREN IDENTIFIER EXCLMARK IDENTIFIER IDENTIFIER RPAREN 
  { AtomR(false, $4, $2, $5) }
| IDENTIFIER EQ IDENTIFIER
  { Eq(true, $1, $3) }
| IDENTIFIER NEQ IDENTIFIER
  { Eq(false, $1, $3) }
;

primary_form: 
  TRUE
    { NegFm(falseFm) }
| FALSE
    { falseFm }
| LPAREN form RPAREN
    { $2 }
| fact
    { FactFm($1) }
;

unary_form:
  primary_form
    { $1 }
| EXCLMARK primary_form
    { NegFm($2) }
;

and_form:
    unary_form
    { $1 }
| unary_form BLAND and_form
  { BinopFm(Conj, $1, $3) }
;

or_form: 
    and_form 
    { $1 }
| and_form BLOR or_form
    { BinopFm(Disj, $1, $3) }
;


form:
    or_form
    { $1 }
;


/* *******  STATEMENTS  ******* */

stmt: 
    compound_stmt { $1 }
|   del_stmt  { $1 }
|   add_stmt  { $1 }
|   select_stmt { $1 }
|   if_stmt  { $1 }
|   iteration_stmt { $1 }
;

compound_stmt: 
    LBRACE block_item_list_opt RBRACE
    { $2 }
;

block_item_list_opt:
    /* empty */
  { Skip }
| block_item_list
    { $1 }
;

block_item_list: 
    stmt
    { $1 }
| block_item_list stmt
    {Seq ($1, $2)}
;

del_stmt: 
  DELETE LPAREN IDENTIFIER IDENTIFIER IDENTIFIER RPAREN SEMICOLON 
  { EDel($3, $4, $5) }
| DELETE LPAREN IDENTIFIER COLON IDENTIFIER RPAREN SEMICOLON 
  { NDel($3, $5) }
;

add_stmt: 
  ADD LPAREN IDENTIFIER IDENTIFIER IDENTIFIER RPAREN SEMICOLON 
  { EAdd($3, $4, $5) }
| ADD LPAREN IDENTIFIER COLON IDENTIFIER RPAREN SEMICOLON 
  { NAdd($3, $5) }
;

select_stmt: 
  SELECT IDENTIFIER WITH form SEMICOLON 
  { SelAss($2, $4) }
;


/* grammar disambiguated by %prec declaration */
if_stmt: 
  IF LPAREN form RPAREN stmt %prec ELSE
  { If($3, $5, Skip) }
| IF LPAREN form RPAREN stmt ELSE stmt 
  { If($3, $5, $7) }
;

/* TODO: unpleaseant inversion of order between concrete and abstract syntax */
iteration_stmt: 
  WHILE LPAREN form RPAREN invariant stmt 
  { While($5, $3, $6) }
;


