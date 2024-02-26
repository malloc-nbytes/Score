# Score

Score is a compiled language written in OCaml QBE.

See `example.scr` for what is currently possible.

# Requirements
- OCaml https://ocaml.org/
- QBE https://c9x.me/compile/

# Running It

## Build
```
cd ./Score/src/
./clean.sh
./build.sh
```

## Running
```
cd ./Score/src/
touch input.txt
./main
./out
```
Currently, this program will read `./src/input.txt` for the source code
and will create an executable `./src/out`.

# TODO
- Printing [X]
- Integer Types [X]
- String Types [X]
- Functions [X]
- FizzBuzz [X]
- Fix the syntax for `if-else-if` statements [ ]
- `for` loops [ ]
- Arrays [ ]
- Semantic analysis [ ]
- ADTs [ ]
- Turing complete [ ]
- Self-hosted [ ]

# Current Bugs
- Having a `break` statement inside of multiple `while` loops does not function correctly.
- Returning a string from a function results in a segmentation fault.

