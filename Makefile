all: graphprover

# nums.cma: required for Big_int
# the compilation command before ejection of Big_int was:
#	ocamlc -o graphprover  nums.cma $^

graphprover: verif_condition.cmo auxil.cmo lang.cmo parser.cmo lexer.cmo display_dot.cmo improver_simplified.cmo graphprover.cmo
	ocamlc -o graphprover nums.cma $^

verifytrans: verif_condition.cmo lang.cmo parser.cmo lexer.cmo tptp_syntax.cmo
	ocamlc -o verifytrans  nums.cma $^


# Compilation of .ml files

# eliminating generated Big_int
#verif_condition.cmo: verif_condition.ml 
#	./big_int_to_int_script; ocamlc -c $<

# keeping generated Big_int
verif_condition.cmo: verif_condition.ml
	ocamlc -c $<

auxil.cmo: auxil.ml verif_condition.cmo
	ocamlc -c $<

lang.cmo: lang.ml verif_condition.cmo auxil.cmo
	ocamlc -c $<

display_dot.cmo: display_dot.ml lang.cmo verif_condition.cmo
	ocamlc -c $<

improver.cmo: improver.ml verif_condition.cmo auxil.cmo lang.cmo
	ocamlc -c $<

improver_simplified.cmo: improver_simplified.ml verif_condition.cmo auxil.cmo lang.cmo
	ocamlc -c $<

graphprover.cmo: graphprover.ml verif_condition.cmo auxil.cmo lang.cmo parser.cmo display_dot.cmo improver_simplified.cmo
	ocamlc -c $<

# currently not used:
tptp_syntax.cmo: tptp_syntax.ml verif_condition.cmo lang.cmo parser.cmo
	ocamlc -c $<


##### ocamlyacc parser

# ocaml lexer and parser
lexer.ml: lexer.mll lang.cmo
	ocamllex $<

parser.ml parser.mli: parser.mly lang.cmo
	ocamlyacc -v $<

lexer.cmo: lexer.ml parser.cmo
	ocamlc -c $<
parser.cmo: parser.ml parser.cmi lang.cmo verif_condition.cmo
	ocamlc -c $<

#### Generic rules

%.cmi: %.mli
	ocamlc -c $<


.PHONY: clean

clean: 
	rm lexer.ml parser.ml *.mli *.cmi *.cmo *.o
