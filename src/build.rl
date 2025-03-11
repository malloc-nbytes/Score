#!/usr/local/bin/earl

module Main

set_flag("--show-lets", "-x");

let run = len(argv()) > 1 && argv()[1] == "run";
let CXX_FILES = "*.cxx include/ds/*.hxx";
let LLVM_CONFIG = "llvm-config --cxxflags --ldflags --system-libs --libs core";
let CXX_FLAGS = "-I/usr/local/include/llvm/ -Iinclude/ -pedantic -ggdb -DDEBUG -O0 -Wextra -Wall -pedantic -std=c++17 -o main";
let CXX = "g++";

$f"{CXX} {CXX_FLAGS} {CXX_FILES} `{LLVM_CONFIG}`";

if run {
    $"./main";
}
