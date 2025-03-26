#!/usr/local/bin/earl

module Test

import "std/system.rl"; as sys

set_flag("-e");

fn build_compiler() {
    ```
    cd ../
    ./build.sh
    ```;
}

fn cleanup_test_env() {
    let files = sys::ls(".").filter(!= __FILE__);
    foreach f in files {
        let parts = sys::name_and_ext(f);
        if parts[0].is_none() && parts[1].is_some() { # no file extension
            $f"rm {f}";
        } else if parts[1] == some("asm") || parts[1] == some("o") { # output file
            $f"rm {f}";
        }
    }
}

fn compile(files) {
    let prog_names = [];
    foreach f in files {
        println(f"Compiling: {f}...");
        let prefix = sys::name_and_ext(f)[0].unwrap();
        $"../scr " + (case len(argv()) > 1 && argv()[1] == "asm" of {
            true = "--no-cleanup"; _ = "";
        }) + f" -o {prefix} {f} ../std/*.scr";
        prog_names += [prefix];
    }
    return prog_names;
}

fn run(prog_names) {
    foreach f in prog_names {
        println(f"Running: {f}");
        $f"./{f}";
    }
}

build_compiler();
cleanup_test_env();

with files = sys::ls(".").filter(!= __FILE__),
     prog_names = compile(files) in
run(prog_names);
