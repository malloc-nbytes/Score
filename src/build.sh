#!/bin/bash

set -xe

ocamlc -c token.mli
ocamlc -c token.ml
ocamlc -o main token.cmo main.ml
