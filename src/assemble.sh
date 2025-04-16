#!/bin/bash

set -xe

nasm -f elf64 -g -F dwarf output.asm -o output.o
ld -dynamic-linker /lib64/ld-linux-x86-64.so.2 -lc -o output output.o
# gcc -g -O0 -no-pie output.o -o output
