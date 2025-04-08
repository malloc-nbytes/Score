#!/bin/bash

set -xe

nasm -f elf64 -g -F dwarf input.asm -o input.o
gcc -g -O0 -no-pie input.o -o input
