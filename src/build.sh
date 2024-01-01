#!/bin/bash

set -xe

if [ "$1" == "clean" ];
then
    rm *.cmo *.cmi main
else
    ocamlc -c token.mli
    ocamlc -c token.ml
    ocamlc -c lexer.mli
    ocamlc -c lexer.ml
    ocamlc -c ast.mli
    ocamlc -c ast.ml
    ocamlc -c parser.mli
    ocamlc -c parser.ml
    ocamlc -o main token.cmo ast.cmo lexer.cmo parser.cmo main.ml
fi
