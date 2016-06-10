OCAMLC=ocamlc
OCAMLBUILD= ocamlbuild -classic-display \
						-use-menhir -menhir "menhir -v --external-tokens Lexer" \
						-use-ocamlfind -package ocamlgraph -package unix -package str

FLAGS=-Is application,lib,parser

TARGET=application/reactiveIS.ml

BIN_NAT=$(patsubst %.ml, %.native, $(TARGET))

all: native

native:
					$(OCAMLBUILD) $(FLAGS) $(BIN_NAT)
					cp $(addprefix _build/, $(BIN_NAT)) ..

clean:
					$(OCAMLBUILD) -clean

cleanall: clean
					find . -iname *~ -exec rm {} \;

realclean: cleanall

.PHONY: all byte native clean cleanall
