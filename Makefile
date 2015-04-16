all: 
	ocamlbuild -yaccflag -v -libs unix,str main.native; ln -fs main.native toto

print:
	ocamlbuild -yaccflag -v -libs unix,str print.native

byte:
	ocamlbuild -yaccflag -v main.byte

clean: 
	ocamlbuild -clean; rm toto
