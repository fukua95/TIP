.PHONY: all clean byte native profile debug sanity

OCB_FLAGS = -use-ocamlfind -use-menhir -I ast -I interpreter
OCB = ocamlbuild $(OCB_FLAGS)

all: native byte

clean:
	$(OCB) -clean

native: sanity
	$(OCB) tip.native

byte: sanity
	$(OCB) tip.byte

profile: sanity
	$(OCB) -tag profile tip.native

debug: sanity
	$(OCB) -tag debug tip.byte

sanity:
	which menhir
	ocamlfind query batteries
