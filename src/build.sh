#!/bin/bash

set -xe

ocamlc -annot -c utils.mli
ocamlc -annot -c utils.ml
ocamlc -annot -c token.mli
ocamlc -annot -c token.ml
ocamlc -annot -c err.mli
ocamlc -annot -c err.ml
ocamlc -annot -c lexer.mli
ocamlc -annot -c lexer.ml
ocamlc -annot -c ast.mli
ocamlc -annot -c ast.ml
ocamlc -annot -c parser.mli
ocamlc -annot -c parser.ml
ocamlc -annot -c scope.mli
ocamlc -annot -c scope.ml
ocamlc -annot -c ir2.mli
ocamlc -annot -c ir2.ml
ocamlc -annot -c main.ml
ocamlc -o scr utils.cmo token.cmo err.cmo ast.cmo scope.cmo ir2.cmo lexer.cmo parser.cmo main.cmo
