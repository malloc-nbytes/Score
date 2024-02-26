#!/bin/bash

set -xe

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
ocamlc -c il.mli
ocamlc -c il.ml
ocamlc -c main.ml
ocamlc -o scr token.cmo err.cmo ast.cmo il.cmo lexer.cmo parser.cmo main.cmo
