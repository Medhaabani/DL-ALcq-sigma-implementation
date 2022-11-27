
open Verif_condition.VerifCondition ;;
open Auxil ;;
open Lang ;;
open Improver_simplified ;;

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


let extract_vcs_prog (Prog(decls, pre, c, post)) = 
  extract_vcs (qform_of_form pre) c (qform_of_form post)
;;

(*
  qconjFm (vc c (qform_of_form post), 
           qimplFm (qform_of_form pre, wp_dl c (qform_of_form post)))
*)

(* TODO: not used any longer; problems with constructors Set / Coset
let new_var_string_set sugg = function
  | Coset xs -> failwith "new_var_string with Coset"
  | Set xs -> 
    let frees = List.filter (function Free v-> true | Bound n -> false) xs in
    let freenames = List.map (function Free v-> v | Bound n -> failwith "impossible") frees in
    Free (new_var_string sugg freenames)
;;
 *)

(* TODO: Still needed ???
let new_var_string_list sugg xs =
    let frees = List.filter (function Free v-> true | Bound n -> false) xs in
    let freenames = List.map (function Free v-> v | Bound n -> failwith "impossible") frees in
    Free (new_var_string sugg freenames)
;;
*)

let show_vcs infile = print_qform (extract_vcs_prog (parse infile))
;;

let generate_and_prove infile = 
(*
  let eqs = {equal = (=)} in 
*)
  let vcs = (extract_vcs_prog (parse infile)) in
  let neg_prenex_f = (neg_norm_form false (free_prenex_form_list_string vcs)) in 
  let init_branch = [neg_prenex_f] in 
(*
  prove init_branch
*)
  prove_pt init_branch
;;

(* let init_tab = [[neg_prenex_f]] in     
dfs eqs eqs (nvs, eqs) init_tab *)

let print_abox abx = 
  List.iter (fun f -> print_string ((print_form f) ^ "\n")) (all_forms abx)
;;

let prove_and_display filename_prefix maxmod  = 
  let infile = filename_prefix ^ ".trans" in
  print_string (show_vcs infile);
  print_string "\nstarting proof ...";
  flush stdout;
  match generate_and_prove infile with
(*
  | [] -> print_string "formula valid\n"
  | [md] -> 
    print_string "formula not valid\n";
    print_abox md;
    List.iteri 
      (Display_dot.generate_dot_file filename_prefix (maxmod - 1))
      (List.map all_forms [md])
  | _ -> failwith "more than one open branch returned from search"
*)

  | TablUnsatisf _ -> print_string "formula valid\n"
  | TablSatisf md -> 
    print_string "formula not valid\n";
    print_abox md;
    List.iteri 
      (Display_dot.generate_dot_file filename_prefix (maxmod - 1)) 
      (List.map all_forms [md])
;;

let prove_and_display_one filename_prefix  = 
  let infile = filename_prefix ^ ".trans" in
  print_string (show_vcs infile);
  print_string "\nstarting proof ...";
  flush stdout;
  match generate_and_prove infile with
  | TablUnsatisf _ ->
    print_string "formula valid\n";
    (Display_dot.generate_dot_file_one filename_prefix [])
  | TablSatisf md -> 
    print_string "formula not valid\n";
    print_abox md;
    (Display_dot.generate_dot_file_one filename_prefix (all_forms md))
;;
(* for testing: *)
(*   Display_dot.filter_literals (all_forms md) *)
(*    (Display_dot.nodes_and_arcs (all_forms md)) *)


(* Commented out: generation of multiple models *)
let main () = 
  try
    let filename_prefix = Sys.argv.(1) in 
    (* let maxmod = (if Array.length Sys.argv <= 2 then 1 else (int_of_string Sys.argv.(2))) in 
    prove_and_display_one filename_prefix maxmod; 
*)
    prove_and_display_one filename_prefix; 
    0
  with ParseLexError (exn,(line,cnum,tok,tail)) ->
    (print_string ("Parsing error on line: " ^ (string_of_int line) ^ 
                     " column: " ^ (string_of_int cnum) ^
                     " token: "  ^ tok ^
                     "\nrest: "  ^ tail ^ "\n")
    ;
    1)
;;

main();;
