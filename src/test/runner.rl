#!/usr/local/bin/earl

module Runner

import "std/system.rl"; as sys
import "std/io.rl"; as io
import "std/colors.rl";

set_flag("-e");

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
    println("\tasm   - show failed cases' Score and ASM code");
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
    foreach f in files {
        println(f"[RM] {f}"); $f"rm {f}";
    }
}

fn display_code_for_failed_file(fp) {
    let scr = fp + ".scr";
    let asm = fp + ".asm";

    println("SCORE CODE:");
    let f = open(scr, "r");
    with content = f.read() in
    with lines = content.split("\n") in
    foreach line in lines {
        println("    ", line);
    }
    f.close();

    println("ASM CODE:");
    let f2 = open(asm, "r");
    with content = f2.read() in
    with lines = content.split("\n") in
    foreach line in lines {
        println("    ", line);
    }
    f2.close();
}

fn run(exes, show_asm) {
    let passes, fails = (0, 0);
    @const let success = 69;
    log("=== Running Tests ===", Colors::Te.Bold);
    foreach e in exes {
        $f"./{e} || echo $?" |> let _out;
        if (len(_out) == 0) {
            println(f"└──FAILED: {e} (no output)");
            fails += 1;
        } else {
            let out = int(_out);
            if (out != success) {
                log(f"└──FAILED: {e} [exit code {out}]", Colors::Tfc.Red);
                if (show_asm) {
                    display_code_for_failed_file(e);
                }
                fails += 1;
            } else {
                log(f"└──PASSED: {e}", Colors::Tfc.Green);
                passes += 1;
            }
        }
    }
    log("=== Results ===", Colors::Te.Bold);
    log(f"Passed: {passes}", Colors::Te.Invert + Colors::Tfc.Green);
    log(f"Failed: {fails}", Colors::Te.Invert + Colors::Tfc.Red);
}

fn compile() {
    let files = get_test_files();
    let exes = [];

    log("=== Compiling ===", Colors::Te.Bold);
    foreach f in files {
        let stripped = sys::name_and_ext(f);
        let name = stripped[0].unwrap();
        $f"../scr -o {name} {f}";
        exes.append(name);
    }

    return exes;
}

fn main() {
    let show_asm = false;

    if (len(argv()) > 1) {
        if (argv()[1] == "clean") {
            cleanup();
            exit(0);
        } else if (argv()[1] == "asm") {
            show_asm = true;
        } else {
            usage();
        }
    }

    cleanup();
    let exes = compile();
    run(exes, show_asm);
}

main();
