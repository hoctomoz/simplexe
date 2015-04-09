all: 
	ocamlbuild -yaccflag -v -lib unix main.native; ln -fs main.native toto

byte: 
	ocamlbuild -yaccflag -v main.byte

clean: 
	ocamlbuild -clean; rm toto
