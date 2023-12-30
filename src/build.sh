#!/bin/bash

set -xe

ocamlc -c token.mli
ocamlc -c token.ml
ocamlc -c lexer.mli
ocamlc -c lexer.ml
ocamlc -o main token.cmo lexer.cmo main.ml
