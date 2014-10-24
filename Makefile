FLAGS = -inline 1000 -nodynlink -fno-PIC
all:
	ocamlopt -c utils.ml $(FLAGS)
	ocamlopt -c encrypt.ml $(FLAGS)
	ocamlopt -c decrypt.ml $(FLAGS)
	ocamlopt -c mitm.ml $(FLAGS)
	ocamlopt utils.cmx encrypt.cmx decrypt.cmx mitm.cmx -o mitm $(FLAGS)

map:
	ocamlopt -c utils.ml $(FLAGS)
	ocamlopt -c encrypt.ml $(FLAGS)
	ocamlopt -c decrypt.ml $(FLAGS)
	ocamlopt -c mitm_map.ml $(FLAGS)
	ocamlopt utils.cmx encrypt.cmx decrypt.cmx mitm_map.cmx -o mitm_map $(FLAGS)
	

clean:
	rm -f *.cmi *.cmx *.o mitm mitm_map
