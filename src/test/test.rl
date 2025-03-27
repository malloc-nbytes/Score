#!/usr/local/bin/earl

# This is the entrypoint for the Score tests.
# This file is written in EARL and can be found
# here: https://github.com/malloc-nbytes/EARL/

module Test

import "std/system.rl"; as sys

set_flag("-xe");

assert(__OS__ == "LINUX");

fn build_compiler() {
    $"cd ../ && ./build.sh";
    ```
    cd ../
    ./build.sh
    ```;
}

fn get_usable_files() {
    sys::ls(".").filter(|f| {
        # We do not want the current file
        # or any directories.
        f != __FILE__ && !sys::isdir(f);
    });
}

fn cleanup_test_env() {
    let files = get_usable_files();
    foreach f in files {
        if sys::isdir(f) { continue; } # Skip artifacts
        let parts = sys::name_and_ext(f);
        if parts[0].is_none() && parts[1].is_some() { # no file extension, is executable
            $f"rm {f}";
        } else if parts[1] == some("asm") || parts[1] == some("o") { # scr compiler output file
            $f"rm {f}";
        }
    }
}

fn compile(files: list, asm: bool) {
    let special_test_deps = {
        "./test-imports": "./test-artifacts/imports-artifacts.scr"
    };

    let prog_names = [];

    foreach f in files {
        println(f"Compiling: {f}...");

        let prefix = sys::name_and_ext(f)[0].unwrap();
        let basic_compile_cmd = f"../scr -o {prefix} {f} ../std/*.scr";
        let debug_compile_cmd = f"../scr --no-cleanup -o {prefix} {f} ../std/*.scr";

        let cmd = "";

        if special_test_deps[prefix] {
            cmd = case asm of {
                true = debug_compile_cmd;
                _    = basic_compile_cmd;
            } + " " + special_test_deps[prefix].unwrap();
        }
        else if asm {
            cmd = debug_compile_cmd;
        }
        else {
            cmd = basic_compile_cmd;
        }

        $cmd;
        prog_names += [prefix];
    }

    prog_names;
}

fn run(prog_names) {
    foreach p in prog_names {
        println(f"Running: {p}");
        $p;
    }
}

build_compiler();
cleanup_test_env();

with asm = len(argv()) > 1 && argv()[1] == "asm",
     files = get_usable_files(),
     prog_names = compile(files, asm) in

run(prog_names);
