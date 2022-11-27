(*

#use "verif_condition.ml" ;;
open VerifCondition ;;
#use "tptp_syntax.ml" ;;
*)

#load "nums.cma" ;;
#load "verif_condition.cmo" ;;
#load "auxil.cmo" ;;
#load "lang.cmo" ;;
#load "parser.cmo" ;;
#load "lexer.cmo" ;;
#load "display_dot.cmo" ;;
#load "improver_simplified.cmo" ;;
#use "graphprover.ml" ;;

(*
#load "tptp_syntax.cmo" ;;
*)

(*
let parse infile = 
  let lexbuf = Lexing.from_channel (open_in infile) in
  Parser.start Lexer.token lexbuf ;;
*)

