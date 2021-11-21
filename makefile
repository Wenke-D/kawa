
dep = kawa2pimp.ml pimp.ml aux.ml aux_type.ml

all: kawac kawai

%: %.native
	mv $^ $@

%.native: %.ml $(dep)
	ocamlbuild $@ -tag 'debug'




clean:
	rm -rf test/*.pmp
	rm -rf *.native
	rm -rf _build