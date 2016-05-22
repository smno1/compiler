TARGETS = bean
TARGETS_BYTE=$(TARGETS:%=%.byte)

LIBS = str

MODULES = bean_ast bean_lex bean_parse symbol analyze type_checking codegen
MLFILES = $(addsuffix .ml, $(MODULES))
CMOFILES = $(addsuffix .cmo, $(MODULES))
CMXFILES = $(addsuffix .cmx, $(MODULES))

ALLMODULES = $(MODULES) bean

OCAMLLEX = ocamllex
OCAMLYACC = ocamlyacc
OCAMLDEP = ocamldep

OCAMLFLAGS =

all : opt byte
byte: $(TARGETS_BYTE)
opt: $(TARGETS)

%.cmi: %.mli
	ocamlc $(OCAMLFLAGS) -c str.cma $<

%.cmo: %.ml
	ocamlc $(OCAMLFLAGS) -g -c str.cma $<

%.cmx: %.ml
	ocamlopt $(OCAMLOPTFLAGS) -g -c str.cmxa $<

%.ml: %.mll
	$(OCAMLLEX) $^

%.ml %.mli: %.mly
	$(OCAMLYACC) $^

bean.byte : $(CMOFILES) bean.cmo
	ocamlc -g -o $@ str.cma $^

bean : $(CMXFILES) bean.cmx
	ocamlopt -g -o $@ str.cmxa $^

clean :
	rm -f *.cmo *.cmi *.cmx *.o
	rm -f bean_lex.ml bean_parse.ml bean_parse.mli

clobber : clean
	rm -f $(TARGETS) $(TARGETS_BYTE)

.PHONY : clean clobber depend

# include depend
depend: bean_lex.ml bean_parse.ml analyze.ml
	$(OCAMLDEP) bean.ml bean.mli $(ALLMODULES:%=%.mli) $(ALLMODULES:%=%.ml) >Makefile.depend
# depend: symbol_table.ml analyze.ml
# 	$(OCAMLDEP) bean.ml bean.mli $(ALLMODULES:%=%.mli) $(ALLMODULES:%=%.ml) >Makefile.depend

-include Makefile.depend
