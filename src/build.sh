#!/bin/bash

set -xe

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
ocamlc -annot -c ir.mli
ocamlc -annot -c ir.ml
ocamlc -annot -c main.ml
ocamlc -o scr token.cmo err.cmo ast.cmo ir.cmo lexer.cmo parser.cmo main.cmo
