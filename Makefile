all: tgen.exe

# These must be in the right order--no forward refs
FILES = mcb.ml cfg.ml grammar.ml combine.ml 

tgen.exe: $(FILES)
	ocamlc -g -o tgen.exe unix.cma str.cma $(FILES)

clean: 
	rm -f tgen.exe *.cmi *.cmo
