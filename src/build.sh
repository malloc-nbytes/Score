#!/bin/bash

set -xe

if [ "$1" == "s" ]
then
    opam switch 5.0.0
    eval $(opam env)
elif [ "$1" == "c" ]
then
    rm *.cmo *.cmi main
else
    ocamlc -c token.mli
    ocamlc -c token.ml
    ocamlc -c err.mli
    ocamlc -c err.ml
    ocamlc -c lexer.mli
    ocamlc -c lexer.ml
    ocamlc -c ast.mli
    ocamlc -c ast.ml
    ocamlc -c parser.mli
    ocamlc -c parser.ml
    ocamlc -c opcode.mli
    ocamlc -c opcode.ml
    ocamlc -c gen.mli
    ocamlc -c gen.ml
    ocamlc -c main.ml
    ocamlc -o main token.cmo err.cmo ast.cmo opcode.cmo gen.cmo lexer.cmo parser.cmo main.cmo
fi
