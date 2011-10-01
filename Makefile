
PACKAGES=bitstring,bitstring.syntax

OCAMLC=ocamlfind ocamlc -package $(PACKAGES) -syntax camlp4o
OCAMLOPT=ocamlfind ocamlopt -package $(PACKAGES) -syntax camlp4o
OCAMLOPTFLAGS=

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c -o $@ $<

%.cmi: %.mli
	$(OCAMLC) -c -o $@ $<

test: frame.cmx
	$(OCAMLOPT) -o test -linkpkg $<

