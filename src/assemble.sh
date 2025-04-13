#!/bin/bash

set -xe

nasm -f elf64 -g -F dwarf output.asm -o output.o
ld -o output output.o
# gcc -g -O0 -no-pie output.o -o output
