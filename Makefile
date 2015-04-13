all: 
	ocamlbuild -yaccflag -v -lib unix main.native; ln -fs main.native toto

print:
	ocamlbuild -yaccflag -v -lib unix print.native

byte:
	ocamlbuild -yaccflag -v main.byte

clean: 
	ocamlbuild -clean; rm toto
