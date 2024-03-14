#!/bin/bash

set -e

green="\033[0;32m"
nc="\033[0m"

echo -e "Building Score"

files=("token.mli" "token.ml"
       "token.mli" "token.ml"
       "utils.mli" "utils.ml"
       "err.mli" "err.ml"
       "lexer.mli" "lexer.ml"
       "ast.mli" "ast.ml"
       "parser.mli" "parser.ml"
       "scope.mli" "scope.ml"
       "emit.mli" "emit.ml"
       "ir.mli" "ir.ml")

for ((i = 0; i < ${#files[@]}; i+=2)); do
    mli_file="${files[i]}"
    ml_file="${files[i+1]}"
    echo -e "${green}[OCAMLC]${nc} $mli_file $ml_file"
    ocamlc -annot -c "$mli_file" "$ml_file"
done

echo -e "${green}[OCAMLC]${nc} main.ml" && ocamlc -annot -c main.ml

cmos="token.cmo utils.cmo err.cmo ast.cmo scope.cmo emit.cmo ir.cmo lexer.cmo parser.cmo main.cmo"

echo -e "${green}[OCAMLC]${nc} $cmos" && ocamlc -o scr $cmos
