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
ocamlc -annot -c preprocessor.mli
ocamlc -annot -c preprocessor.ml
ocamlc -annot -c parser.mli
ocamlc -annot -c parser.ml
ocamlc -annot -c scope.mli
ocamlc -annot -c scope.ml
ocamlc -annot -c utils.mli
ocamlc -annot -c utils.ml
ocamlc -annot -c emit.mli
ocamlc -annot -c emit.ml
ocamlc -annot -c proc.mli
ocamlc -annot -c proc.ml
ocamlc -annot -c ir.mli
ocamlc -annot -c ir.ml
ocamlc -annot -c main.ml
ocamlc -o scr token.cmo preprocessor.cmo err.cmo ast.cmo scope.cmo utils.cmo emit.cmo proc.cmo ir.cmo lexer.cmo parser.cmo main.cmo
