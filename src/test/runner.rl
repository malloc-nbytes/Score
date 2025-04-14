module Runner

import "std/system.rl"; as sys
import "std/io.rl"; as io
import "std/colors.rl";

### This file is the test runner for the Score tests.
### It is required that EARL is installed: https://github.com/malloc-nbytes/EARL/
### No third-party modules need to be installed, just the StdLib.

fn log(msg, c) {
    println(c, msg, Colors::Te.Reset);
}

fn usage() {
    println("Usage: earl runner.earl -- [options...]");
    println("Options:");
    println("\thelp  - print this message");
    println("\tclean - clean all test artifacts");
    exit(0);
}

fn get_test_files() {
    return sys::get_all_files_by_ext(".", "scr");
}

fn cleanup() {
    let files = sys::ls(".").filter(|k| {
        with parts = sys::name_and_ext(k) in
        return !parts[0]
            || (parts[1].unwrap() == "asm"
                || parts[1].unwrap() == "o");
    });
    log(f"Removing: {files}", Colors::Tfc.Yellow);
    foreach f in files {
        $f"rm {f}";
    }
}

fn run(exes) {
    @const let success = 69;
    foreach e in exes {
        $f"./{e} || echo $?" |> let _out;
        if (len(_out) == 0) {
            println(f"FAILED: {e}");
        } else {
            let out = int(_out);
            if (out != success) {
                log(f"FAILED: {e}", Colors::Tfc.Red);
            } else {
                log(f"PASSED: {e}", Colors::Tfc.Green);
            }
        }
    }
}

fn compile() {
    let files = get_test_files();
    let exes = [];

    log(f"Compiling: {files}", Colors::Tfc.Yellow);

    foreach f in files {
        let stripped = sys::name_and_ext(f);
        let name = stripped[0].unwrap();
        $f"../scr -o {name} {f}";
        exes.append(name);
    }

    return exes;
}

fn main() {
    if (len(argv()) > 1) {
        if (argv()[1] == "clean") {
            cleanup();
            exit(0);
        } else {
            usage();
        }
    }

    cleanup();
    let exes = compile();
    run(exes);
}

main();
