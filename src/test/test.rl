#!/usr/local/bin/earl

# This is the entrypoint for the Score tests.
# This file is written in EARL and can be found
# here: https://github.com/malloc-nbytes/EARL/

module Test

import "std/system.rl"; as sys

set_flag("-e");

assert(__OS__ == "LINUX");

fn build_compiler() {
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
        let basic_compile_cmd = f"../scr -o {prefix} {f} ./test-artifacts/test-utils.scr";
        let debug_compile_cmd = f"../scr --no-cleanup -o {prefix} {f} ./test-artifacts/test-utils.scr";

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

fn usage() {
    println("Usage: ", argv()[0], " [options]");
    println("Options:");
    println("  help  - show this message");
    println("  asm   - do not cleanup asm files");
    println("  clean - clean up all generated files");
    exit(0);
}

with A = len(argv()) > 1 in
let asm, clean, help_ = (
    A && argv()[1] == "asm",
    A && argv()[1] == "clean",
    A && argv()[1] == "help",
);

@world fn main() {
    if help_ {
        usage();
    } else if clean {
        cleanup_test_env();
        exit(0);
    } else {
        build_compiler();
        cleanup_test_env();

        with files = get_usable_files(),
        prog_names = compile(files, asm) in
        run(prog_names);
    }
}

main();
