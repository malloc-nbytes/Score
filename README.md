# Score

Score is a compiled language written in OCaml with the QBE backend.

See `./src/examples` for what is currently possible.

# Requirements
- OCaml https://ocaml.org/
- QBE https://c9x.me/compile/
- gcc https://gcc.gnu.org/

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
./scr <input filepath>
```

## Tests
```
cd ./Score/src/
./test.sh
```
# TODO
- Integer Types [X]
- String Types [X]
- Functions [X]
- Printing [X]
- FizzBuzz [X]
- Scope [X]
- Fix the syntax for `if-else-if` statements [X]
- `for` loops [X]
- Arrays [X]
- Turing complete [X]
- Imports [X]
- Self-hosted tests [X]
- structs [X]
- unary operators [ ]
- break statements [ ]
- Self-hosted [ ]
