# Score

Score is a compiled language written in OCaml with the QBE backend.

See `example.scr` or `./src/tests/` for what is currently possible.

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
./scr <input filepath>
```
or
```
./scr -o <output filepath> <input filepath>
```
See `./scr --help` for more options

# TODO
- Integer Types [X]
- String Types [X]
- Functions [X]
- Printing [X]
- FizzBuzz [X]
- Scope [X]
- Fix the syntax for `if-else-if` statements [X]
- `for` loops [X]
- unary operators [ ]
- Arrays [ ]
- Semantic analysis [ ]
- ADTs [ ]
- Turing complete [ ]
- Self-hosted [ ]

# Current Bugs
- Having a `break` statement inside of multiple `while` loops does not function correctly.
- Having a `return` statement in the middle of a procedure does not work.

