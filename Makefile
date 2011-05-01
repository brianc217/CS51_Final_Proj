all: tgen.exe

# These must be in the right order--no forward refs
FILES = dict.ml markov.ml combination.ml tgen.ml

tgen.exe: $(FILES)
	ocamlc -g -o tgen.exe unix.cma str.cma $(FILES)