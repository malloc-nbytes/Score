# Score

Score is a compiled language written in OCaml QBE.

See `example.scr` for what is currently possible.

# Current Plans
- FizzBuzz [X]
- Printing [X]
- Integer Types [X]
- String Types [X]
- Functions [X]
- Turing complete []
- Self-hosted []

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



