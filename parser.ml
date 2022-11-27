type token =
  | IDENTIFIER of (string)
  | INTCONSTANT of (int)
  | STRINGCONSTANT of (string)
  | CHARCONSTANT of (string)
  | LPAREN
  | RPAREN
  | DOT
  | NEW
  | CLASS
  | THIS
  | NULL
  | VOID
  | LSQBRACE
  | RSQBRACE
  | EXCLMARK
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | MOD
  | LBRACE
  | RBRACE
  | EQ
  | NEQ
  | COMMA
  | SEMICOLON
  | COLON
  | QMARK
  | IF
  | ELSE
  | WHILE
  | FOR
  | RETURN
  | BCEQ
  | BCGE
  | BCGT
  | BCLE
  | BCLT
  | BCNE
  | BLAND
  | BLOR
  | SYNCHRONIZED
  | DO
  | EOF
  | PRE
  | POST
  | INV
  | VARS
  | CONCEPTS
  | ROLES
  | SELECT
  | WITH
  | DELETE
  | ADD
  | TRUE
  | FALSE
  | CONCTOP
  | CONCBOT
  | CONCALL
  | CONCEX
  | CONCAND
  | CONCOR

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"

open Verif_condition.VerifCondition ;;
open Lang ;;

let parse_error p = (* Called by the parser function on error *)
  print_endline p;
  flush stdout
;;

# 78 "parser.ml"
let yytransl_const = [|
  261 (* LPAREN *);
  262 (* RPAREN *);
  263 (* DOT *);
  264 (* NEW *);
  265 (* CLASS *);
  266 (* THIS *);
  267 (* NULL *);
  268 (* VOID *);
  269 (* LSQBRACE *);
  270 (* RSQBRACE *);
  271 (* EXCLMARK *);
  272 (* PLUS *);
  273 (* MINUS *);
  274 (* TIMES *);
  275 (* DIV *);
  276 (* MOD *);
  277 (* LBRACE *);
  278 (* RBRACE *);
  279 (* EQ *);
  280 (* NEQ *);
  281 (* COMMA *);
  282 (* SEMICOLON *);
  283 (* COLON *);
  284 (* QMARK *);
  285 (* IF *);
  286 (* ELSE *);
  287 (* WHILE *);
  288 (* FOR *);
  289 (* RETURN *);
  290 (* BCEQ *);
  291 (* BCGE *);
  292 (* BCGT *);
  293 (* BCLE *);
  294 (* BCLT *);
  295 (* BCNE *);
  296 (* BLAND *);
  297 (* BLOR *);
  298 (* SYNCHRONIZED *);
  299 (* DO *);
    0 (* EOF *);
  300 (* PRE *);
  301 (* POST *);
  302 (* INV *);
  303 (* VARS *);
  304 (* CONCEPTS *);
  305 (* ROLES *);
  306 (* SELECT *);
  307 (* WITH *);
  308 (* DELETE *);
  309 (* ADD *);
  310 (* TRUE *);
  311 (* FALSE *);
  312 (* CONCTOP *);
  313 (* CONCBOT *);
  314 (* CONCALL *);
  315 (* CONCEX *);
  316 (* CONCAND *);
  317 (* CONCOR *);
    0|]

let yytransl_block = [|
  257 (* IDENTIFIER *);
  258 (* INTCONSTANT *);
  259 (* STRINGCONSTANT *);
  260 (* CHARCONSTANT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\003\000\007\000\008\000\009\000\004\000\006\000\
\012\000\010\000\010\000\013\000\013\000\013\000\013\000\013\000\
\013\000\013\000\013\000\013\000\013\000\013\000\015\000\015\000\
\016\000\016\000\017\000\017\000\014\000\018\000\018\000\018\000\
\018\000\018\000\019\000\019\000\019\000\019\000\020\000\020\000\
\021\000\021\000\022\000\022\000\011\000\023\000\023\000\023\000\
\023\000\023\000\023\000\024\000\005\000\005\000\030\000\030\000\
\025\000\026\000\027\000\028\000\028\000\029\000\000\000"

let yylen = "\002\000\
\001\000\005\000\003\000\003\000\003\000\003\000\004\000\004\000\
\004\000\001\000\003\000\001\000\001\000\001\000\003\000\005\000\
\005\000\006\000\006\000\006\000\006\000\006\000\001\000\002\000\
\001\000\003\000\001\000\003\000\001\000\003\000\005\000\006\000\
\003\000\003\000\001\000\001\000\003\000\001\000\001\000\002\000\
\001\000\003\000\001\000\003\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\003\000\000\000\001\000\001\000\002\000\
\007\000\007\000\005\000\005\000\007\000\006\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\063\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\004\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\055\000\
\046\000\047\000\048\000\049\000\050\000\051\000\000\000\000\000\
\000\000\003\000\011\000\000\000\000\000\000\000\035\000\036\000\
\000\000\038\000\039\000\000\000\000\000\045\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\056\000\005\000\
\000\000\000\000\000\000\000\000\000\000\000\000\040\000\007\000\
\000\000\000\000\052\000\000\000\000\000\000\000\000\000\000\000\
\000\000\002\000\006\000\033\000\034\000\014\000\000\000\000\000\
\012\000\013\000\023\000\030\000\000\000\000\000\029\000\000\000\
\000\000\037\000\042\000\044\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\024\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\059\000\000\000\000\000\008\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\015\000\026\000\028\000\
\031\000\000\000\000\000\000\000\062\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\032\000\061\000\
\000\000\057\000\058\000\000\000\000\000\000\000\000\000\000\000\
\016\000\017\000\009\000\022\000\018\000\019\000\020\000\021\000"

let yydgoto = "\002\000\
\004\000\005\000\006\000\011\000\023\000\054\000\007\000\013\000\
\034\000\009\000\041\000\114\000\083\000\084\000\085\000\086\000\
\087\000\042\000\043\000\044\000\045\000\046\000\024\000\025\000\
\026\000\027\000\028\000\029\000\030\000\031\000"

let yysindex = "\001\000\
\213\254\000\000\007\255\000\000\000\000\233\254\236\254\005\255\
\014\255\015\255\251\254\007\255\253\254\007\255\000\000\017\255\
\251\254\039\255\068\255\081\255\082\255\087\255\045\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\251\254\071\255\
\007\255\000\000\000\000\243\254\024\255\026\255\000\000\000\000\
\073\255\000\000\000\000\054\255\060\255\000\000\086\255\017\255\
\017\255\061\255\104\255\112\255\088\255\118\000\000\000\000\000\
\093\255\119\255\120\255\002\255\083\255\116\255\000\000\000\000\
\017\255\017\255\000\000\117\255\118\255\017\255\124\255\125\255\
\017\255\000\000\000\000\000\000\000\000\000\000\018\255\004\255\
\000\000\000\000\000\000\000\000\067\255\069\255\000\000\127\255\
\128\255\000\000\000\000\000\000\251\254\089\255\105\255\131\255\
\132\255\108\255\134\255\135\255\136\255\137\255\138\255\140\255\
\141\255\139\255\000\000\002\255\002\255\142\255\143\255\113\255\
\122\255\251\254\000\000\144\255\145\255\000\000\146\255\151\255\
\152\255\153\255\154\255\002\255\002\255\000\000\000\000\000\000\
\000\000\150\255\251\254\017\255\000\000\133\255\147\255\002\255\
\002\255\002\255\002\255\002\255\155\255\156\255\000\000\000\000\
\148\255\000\000\000\000\157\255\158\255\159\255\160\255\161\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\149\255\
\000\000\000\000\045\255\000\000\000\000\000\000\000\000\000\000\
\162\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\254\254\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\063\255\085\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\009\255\062\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\064\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\129\000\000\000\000\000\000\000\
\000\000\024\000\220\255\000\000\077\000\183\255\000\000\050\000\
\051\000\000\000\130\000\000\000\104\000\105\000\225\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yytablesize = 184
let yytable = "\055\000\
\062\000\001\000\078\000\003\000\078\000\106\000\079\000\008\000\
\079\000\058\000\059\000\068\000\069\000\060\000\025\000\017\000\
\080\000\036\000\078\000\054\000\010\000\037\000\079\000\018\000\
\061\000\019\000\036\000\012\000\037\000\014\000\037\000\038\000\
\080\000\095\000\025\000\032\000\098\000\035\000\038\000\015\000\
\099\000\016\000\054\000\048\000\020\000\033\000\021\000\022\000\
\025\000\025\000\141\000\142\000\100\000\101\000\102\000\103\000\
\057\000\081\000\082\000\081\000\082\000\112\000\148\000\149\000\
\150\000\151\000\152\000\027\000\041\000\025\000\039\000\040\000\
\049\000\081\000\082\000\104\000\105\000\039\000\040\000\039\000\
\040\000\050\000\133\000\088\000\060\000\060\000\051\000\027\000\
\041\000\053\000\043\000\052\000\060\000\065\000\060\000\145\000\
\056\000\089\000\064\000\144\000\066\000\027\000\027\000\041\000\
\071\000\058\000\059\000\067\000\060\000\060\000\043\000\070\000\
\072\000\060\000\073\000\060\000\060\000\074\000\075\000\076\000\
\077\000\090\000\093\000\094\000\096\000\097\000\108\000\110\000\
\111\000\109\000\115\000\116\000\117\000\118\000\113\000\119\000\
\120\000\121\000\122\000\123\000\124\000\125\000\131\000\130\000\
\126\000\047\000\136\000\129\000\132\000\134\000\135\000\137\000\
\138\000\139\000\140\000\143\000\107\000\127\000\146\000\128\000\
\153\000\154\000\156\000\157\000\158\000\159\000\160\000\063\000\
\091\000\000\000\092\000\000\000\147\000\155\000\010\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\053\000"

let yycheck = "\031\000\
\037\000\001\000\001\001\047\001\001\001\079\000\005\001\001\001\
\005\001\023\001\024\001\048\000\049\000\027\001\006\001\021\001\
\015\001\001\001\001\001\022\001\044\001\005\001\005\001\029\001\
\001\001\031\001\001\001\048\001\005\001\025\001\005\001\015\001\
\015\001\070\000\026\001\012\000\073\000\014\000\015\001\026\001\
\023\001\027\001\045\001\005\001\050\001\049\001\052\001\053\001\
\040\001\041\001\124\000\125\000\035\001\036\001\037\001\038\001\
\033\000\056\001\057\001\056\001\057\001\093\000\136\000\137\000\
\138\000\139\000\140\000\006\001\006\001\061\001\054\001\055\001\
\005\001\056\001\057\001\058\001\059\001\054\001\055\001\054\001\
\055\001\001\001\114\000\001\001\021\001\022\001\005\001\026\001\
\026\001\045\001\006\001\005\001\029\001\040\001\031\001\132\000\
\026\001\015\001\026\001\131\000\041\001\040\001\041\001\041\001\
\001\001\023\001\024\001\022\001\045\001\027\001\026\001\051\001\
\001\001\050\001\027\001\052\001\053\001\000\000\026\001\001\001\
\001\001\006\001\006\001\006\001\001\001\001\001\060\001\001\001\
\001\001\061\001\026\001\001\001\001\001\026\001\046\001\002\001\
\002\001\002\001\002\001\002\001\001\001\001\001\030\001\001\001\
\006\001\017\000\001\001\006\001\027\001\006\001\006\001\001\001\
\001\001\001\001\001\001\006\001\080\000\108\000\026\001\109\000\
\006\001\006\001\006\001\006\001\006\001\006\001\006\001\038\000\
\065\000\255\255\066\000\255\255\026\001\026\001\026\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\022\001"

let yynames_const = "\
  LPAREN\000\
  RPAREN\000\
  DOT\000\
  NEW\000\
  CLASS\000\
  THIS\000\
  NULL\000\
  VOID\000\
  LSQBRACE\000\
  RSQBRACE\000\
  EXCLMARK\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  MOD\000\
  LBRACE\000\
  RBRACE\000\
  EQ\000\
  NEQ\000\
  COMMA\000\
  SEMICOLON\000\
  COLON\000\
  QMARK\000\
  IF\000\
  ELSE\000\
  WHILE\000\
  FOR\000\
  RETURN\000\
  BCEQ\000\
  BCGE\000\
  BCGT\000\
  BCLE\000\
  BCLT\000\
  BCNE\000\
  BLAND\000\
  BLOR\000\
  SYNCHRONIZED\000\
  DO\000\
  EOF\000\
  PRE\000\
  POST\000\
  INV\000\
  VARS\000\
  CONCEPTS\000\
  ROLES\000\
  SELECT\000\
  WITH\000\
  DELETE\000\
  ADD\000\
  TRUE\000\
  FALSE\000\
  CONCTOP\000\
  CONCBOT\000\
  CONCALL\000\
  CONCEX\000\
  CONCAND\000\
  CONCOR\000\
  "

let yynames_block = "\
  IDENTIFIER\000\
  INTCONSTANT\000\
  STRINGCONSTANT\000\
  CHARCONSTANT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'prog) in
    Obj.repr(
# 44 "parser.mly"
            ( _1 )
# 371 "parser.ml"
               : (string, string, string) Lang.prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'allDecls) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'precond) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'block_item_list_opt) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'postcond) in
    Obj.repr(
# 49 "parser.mly"
  (Prog(_1, _2, _3, _4))
# 381 "parser.ml"
               : 'prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'varDecls) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'conceptDecls) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'roleDecls) in
    Obj.repr(
# 54 "parser.mly"
  (Decls(_1, _2, _3))
# 390 "parser.ml"
               : 'allDecls))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'idListCommaSep) in
    Obj.repr(
# 59 "parser.mly"
  (_2)
# 397 "parser.ml"
               : 'varDecls))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'idListCommaSep) in
    Obj.repr(
# 64 "parser.mly"
  (_2)
# 404 "parser.ml"
               : 'conceptDecls))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'idListCommaSep) in
    Obj.repr(
# 69 "parser.mly"
  (_2)
# 411 "parser.ml"
               : 'roleDecls))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'form) in
    Obj.repr(
# 74 "parser.mly"
  (_3)
# 418 "parser.ml"
               : 'precond))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'form) in
    Obj.repr(
# 79 "parser.mly"
  (_3)
# 425 "parser.ml"
               : 'postcond))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'form) in
    Obj.repr(
# 84 "parser.mly"
  (_3)
# 432 "parser.ml"
               : 'invariant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 89 "parser.mly"
([_1])
# 439 "parser.ml"
               : 'idListCommaSep))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'idListCommaSep) in
    Obj.repr(
# 91 "parser.mly"
([_1]@_3)
# 447 "parser.ml"
               : 'idListCommaSep))
; (fun __caml_parser_env ->
    Obj.repr(
# 100 "parser.mly"
    ( Top )
# 453 "parser.ml"
               : 'primary_concept))
; (fun __caml_parser_env ->
    Obj.repr(
# 102 "parser.mly"
    ( Bottom )
# 459 "parser.ml"
               : 'primary_concept))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 104 "parser.mly"
  ( AtomC (true,_1) )
# 466 "parser.ml"
               : 'primary_concept))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'concept) in
    Obj.repr(
# 106 "parser.mly"
  ( _2 )
# 473 "parser.ml"
               : 'primary_concept))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'concept) in
    Obj.repr(
# 110 "parser.mly"
  ( NumRestrC (Lt, Nat 1, _3, (NegC _4)) )
# 481 "parser.ml"
               : 'primary_concept))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'concept) in
    Obj.repr(
# 114 "parser.mly"
  ( NumRestrC (Ge, Nat 1, _3, _4) )
# 489 "parser.ml"
               : 'primary_concept))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'concept) in
    Obj.repr(
# 116 "parser.mly"
  ( NumRestrC (Ge, Nat _3, _4, _5) )
# 498 "parser.ml"
               : 'primary_concept))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'concept) in
    Obj.repr(
# 118 "parser.mly"
  ( NumRestrC (Ge, Nat (_3 + 1), _4, _5) )
# 507 "parser.ml"
               : 'primary_concept))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'concept) in
    Obj.repr(
# 120 "parser.mly"
  ( NumRestrC (Lt, Nat (_3 + 1), _4, _5) )
# 516 "parser.ml"
               : 'primary_concept))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'concept) in
    Obj.repr(
# 122 "parser.mly"
  ( NumRestrC (Lt, Nat _3, _4, _5) )
# 525 "parser.ml"
               : 'primary_concept))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'concept) in
    Obj.repr(
# 124 "parser.mly"
  ( andC(NumRestrC (Ge, Nat _3, _4, _5),
         NumRestrC (Lt, Nat (_3 + 1), _4, _5)) )
# 535 "parser.ml"
               : 'primary_concept))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'primary_concept) in
    Obj.repr(
# 131 "parser.mly"
    ( _1 )
# 542 "parser.ml"
               : 'unary_concept))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'primary_concept) in
    Obj.repr(
# 133 "parser.mly"
    ( NegC(_2) )
# 549 "parser.ml"
               : 'unary_concept))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'unary_concept) in
    Obj.repr(
# 138 "parser.mly"
    ( _1 )
# 556 "parser.ml"
               : 'and_concept))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'unary_concept) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'and_concept) in
    Obj.repr(
# 140 "parser.mly"
    ( andC(_1, _3) )
# 564 "parser.ml"
               : 'and_concept))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'and_concept) in
    Obj.repr(
# 145 "parser.mly"
    ( _1 )
# 571 "parser.ml"
               : 'or_concept))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'and_concept) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'or_concept) in
    Obj.repr(
# 147 "parser.mly"
    ( orC(_1, _3) )
# 579 "parser.ml"
               : 'or_concept))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'or_concept) in
    Obj.repr(
# 152 "parser.mly"
    ( _1 )
# 586 "parser.ml"
               : 'concept))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'concept) in
    Obj.repr(
# 157 "parser.mly"
  ( Inst(_1, _3) )
# 594 "parser.ml"
               : 'fact))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 159 "parser.mly"
  ( AtomR(true, _3, _2, _4) )
# 603 "parser.ml"
               : 'fact))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 161 "parser.mly"
  ( AtomR(false, _4, _2, _5) )
# 612 "parser.ml"
               : 'fact))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 163 "parser.mly"
  ( Eq(true, _1, _3) )
# 620 "parser.ml"
               : 'fact))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 165 "parser.mly"
  ( Eq(false, _1, _3) )
# 628 "parser.ml"
               : 'fact))
; (fun __caml_parser_env ->
    Obj.repr(
# 170 "parser.mly"
    ( NegFm(falseFm) )
# 634 "parser.ml"
               : 'primary_form))
; (fun __caml_parser_env ->
    Obj.repr(
# 172 "parser.mly"
    ( falseFm )
# 640 "parser.ml"
               : 'primary_form))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'form) in
    Obj.repr(
# 174 "parser.mly"
    ( _2 )
# 647 "parser.ml"
               : 'primary_form))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fact) in
    Obj.repr(
# 176 "parser.mly"
    ( FactFm(_1) )
# 654 "parser.ml"
               : 'primary_form))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'primary_form) in
    Obj.repr(
# 181 "parser.mly"
    ( _1 )
# 661 "parser.ml"
               : 'unary_form))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'primary_form) in
    Obj.repr(
# 183 "parser.mly"
    ( NegFm(_2) )
# 668 "parser.ml"
               : 'unary_form))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'unary_form) in
    Obj.repr(
# 188 "parser.mly"
    ( _1 )
# 675 "parser.ml"
               : 'and_form))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'unary_form) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'and_form) in
    Obj.repr(
# 190 "parser.mly"
  ( BinopFm(Conj, _1, _3) )
# 683 "parser.ml"
               : 'and_form))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'and_form) in
    Obj.repr(
# 195 "parser.mly"
    ( _1 )
# 690 "parser.ml"
               : 'or_form))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'and_form) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'or_form) in
    Obj.repr(
# 197 "parser.mly"
    ( BinopFm(Disj, _1, _3) )
# 698 "parser.ml"
               : 'or_form))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'or_form) in
    Obj.repr(
# 203 "parser.mly"
    ( _1 )
# 705 "parser.ml"
               : 'form))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'compound_stmt) in
    Obj.repr(
# 210 "parser.mly"
                  ( _1 )
# 712 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'del_stmt) in
    Obj.repr(
# 211 "parser.mly"
              ( _1 )
# 719 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'add_stmt) in
    Obj.repr(
# 212 "parser.mly"
              ( _1 )
# 726 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'select_stmt) in
    Obj.repr(
# 213 "parser.mly"
                ( _1 )
# 733 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'if_stmt) in
    Obj.repr(
# 214 "parser.mly"
             ( _1 )
# 740 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'iteration_stmt) in
    Obj.repr(
# 215 "parser.mly"
                   ( _1 )
# 747 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'block_item_list_opt) in
    Obj.repr(
# 220 "parser.mly"
    ( _2 )
# 754 "parser.ml"
               : 'compound_stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 225 "parser.mly"
  ( Skip )
# 760 "parser.ml"
               : 'block_item_list_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'block_item_list) in
    Obj.repr(
# 227 "parser.mly"
    ( _1 )
# 767 "parser.ml"
               : 'block_item_list_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 232 "parser.mly"
    ( _1 )
# 774 "parser.ml"
               : 'block_item_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'block_item_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 234 "parser.mly"
    (Seq (_1, _2))
# 782 "parser.ml"
               : 'block_item_list))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 239 "parser.mly"
  ( EDel(_3, _4, _5) )
# 791 "parser.ml"
               : 'del_stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 244 "parser.mly"
  ( EAdd(_3, _4, _5) )
# 800 "parser.ml"
               : 'add_stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'form) in
    Obj.repr(
# 249 "parser.mly"
  ( SelAss(_2, _4) )
# 808 "parser.ml"
               : 'select_stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'form) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 256 "parser.mly"
  ( If(_3, _5, Skip) )
# 816 "parser.ml"
               : 'if_stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'form) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 258 "parser.mly"
  ( If(_3, _5, _7) )
# 825 "parser.ml"
               : 'if_stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'form) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'invariant) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 264 "parser.mly"
  ( While(_5, _3, _6) )
# 834 "parser.ml"
               : 'iteration_stmt))
(* Entry start *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let start (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : (string, string, string) Lang.prog)
