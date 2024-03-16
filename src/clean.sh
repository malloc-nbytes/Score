#!/bin/bash

set -xe

rm -f ./examples/*.s ./examples/*.ssa ./examples/*.out
rm -f ./std/*.s ./std/*.ssa ./std/*.out
rm -f ./tests/*.out ./tests/*.ssa ./tests/*.s
rm -f *.cmo *.cmi *.annot *.ssa *.s out* main scr *.out
