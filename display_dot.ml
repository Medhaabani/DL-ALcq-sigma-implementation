open Verif_condition.VerifCondition ;;
open Lang ;;

(* ---------------------------------------------------------------------- *)
(* Generating Dots output *)
(* ---------------------------------------------------------------------- *)

let is_literal = function
  | FactFm (Inst (_, AtomC _)) -> true
  | FactFm (AtomR (true, _, _, _)) -> true
  | _ -> false
;;

let is_inst = function
  | FactFm (Inst (_, _)) -> true
  | _ -> false

let filter_literals abox = List.filter is_literal abox
;;

let group_inst_atomr lits = List.partition is_inst lits
;;

let equal_inst_name x = function
  | FactFm (Inst(y, _)) -> x = y
  | _ -> false
;;

let rec group_inst = function
  | [] -> []
  | FactFm (Inst(x, c)) :: xs -> 
    let (same, other) = List.partition (equal_inst_name x) xs in
    (x, (FactFm (Inst(x, c)) :: same)) :: group_inst other
  | _ -> failwith "non-inst in group_inst"
;;

let rec remove_duplicates = function
  | [] -> []
  | x::xs -> if List.mem x xs then remove_duplicates xs else x::remove_duplicates xs
;;


let nodes_and_arcs abox =
  let lits = filter_literals abox in
  let (insts, atomrs) = group_inst_atomr lits in
  let ginsts = group_inst insts in
  let atomrs_nodes = (List.concat (List.map (function FactFm (AtomR(_, _, n1, n2)) -> [n1; n2] | _ -> []) atomrs)) in
  let all_nodes = remove_duplicates ((List.map fst ginsts) @ atomrs_nodes) in
  (all_nodes, ginsts, atomrs)
;;

let print_node_info = function 
  | FactFm (Inst(_, AtomC(true, n))) ->  n ^ " "
  | FactFm (Inst(_, AtomC(false, n))) ->  "-" ^ n ^ " "
  | _ -> failwith "print_node_info: match failure"

let print_node_infos n ginsts = 
  if List.mem_assoc n ginsts
  then (let infos = List.assoc n ginsts in 
       List.fold_left (^) ": " (List.map print_node_info infos))
  else ""
;;

let print_node_name n = n ;;
(*
function
    | Free s -> s
    | Bound n -> failwith "print_node: bound names not treated"
;;
*)

(* TODO: Bound names not treated correctly *)
let print_node ginsts n = 
  let n_name = print_node_name n in
  let node_info = print_node_infos n ginsts in 
  n_name ^ " [label= \" " ^ n_name ^ node_info ^ " \" ] \n"
;;


let print_nodes all_nodes ginsts = List.fold_left (^) "" (List.map (print_node ginsts) all_nodes)
;;

let print_arc = function
  | FactFm (AtomR (_, reln, n1, n2)) -> 
    (print_node_name n1) ^ " -> " ^ (print_node_name n2) ^ " [label= \" " ^ reln ^ " \" ] \n"
  | _ -> failwith "print_arc: malformed arc"
;;

let print_arcs atomrs = List.fold_left (^) "" (List.map print_arc atomrs)
;;

let print_graph (all_nodes, ginsts, atomrs) grname =
  "digraph " ^ grname ^ " {\n" ^ 
    (print_nodes all_nodes ginsts) ^ 
    (print_arcs atomrs) ^ "}\n"
;;

let generate_dot_file filename_prefix maxmod i md = 
  if i > maxmod
  then ()
  else 
    let graph_name = 
      if maxmod = 0 then filename_prefix else (filename_prefix ^ "_" ^ (string_of_int i)) in
    let dot_filename = graph_name  ^ ".gv" in
    let pdf_filename = graph_name  ^ ".pdf" in
    let outf = open_out dot_filename in
    output_string outf (print_graph (nodes_and_arcs md) filename_prefix);
    close_out outf;
    let _ = Sys.command ("dot " ^ dot_filename ^ " -Tpdf -o " ^ pdf_filename) in
    print_string ("generated " ^ dot_filename ^ " and " ^ pdf_filename ^ "\n")


let generate_dot_file_one filename_prefix md = 
  let dot_filename = filename_prefix  ^ ".gv" in
  let pdf_filename = filename_prefix  ^ ".pdf" in
  let outf = open_out dot_filename in
  output_string outf (print_graph (nodes_and_arcs md) filename_prefix);
  close_out outf;
  let _ = Sys.command ("dot " ^ dot_filename ^ " -Tpdf -o " ^ pdf_filename) in
  print_string ("generated " ^ dot_filename ^ " and " ^ pdf_filename ^ "\n")
