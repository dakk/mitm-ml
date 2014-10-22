FLAGS = -inline 20 -nodynlink
all:
	ocamlopt -c utils.ml $(FLAGS)
	ocamlopt -c encrypt.ml $(FLAGS)
	ocamlopt -c decrypt.ml $(FLAGS)
	ocamlopt -c mitm.ml $(FLAGS)
	ocamlopt utils.cmx encrypt.cmx decrypt.cmx mitm.cmx -o mitm $(FLAGS)

clean:
	rm -f *.cmi *.cmx *.o mitm