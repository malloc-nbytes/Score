#!/usr/local/bin/earl

module Runner

import "std/system.rl"; as sys
import "std/io.rl"; as io
import "std/colors.rl";
import "std/utils.rl";

set_flag("-e");

### This file is the test runner for the Score tests.
### It is required that EARL is installed: https://github.com/malloc-nbytes/EARL/
### No third-party modules need to be installed, just the StdLib.

fn log(msg, c) {
    println(c, msg, Colors::Te.Reset);
}

fn info(msg) {
    log(f"[INFO]: {msg}", Colors::Tfc.Yellow);
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
    for i in 0 to len(exes) {
        let e = exes[i];
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
    if (fails == 0) {
        log(f"Passed: {passes}", Colors::Te.Invert + Colors::Tfc.Green);
    } else {
        log(f"Passed: {passes}", Colors::Te.Bold + Colors::Tfc.Green);
    }
    if (fails != 0) {
        log(f"Failed: {fails}", Colors::Te.Invert + Colors::Tfc.Red);
    } else {
        log(f"Failed: {fails}", Colors::Te.Bold + Colors::Tfc.Red);
    }
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

enum Flag_Type {
    Help = 1 << Utils::iota(),
    Clean = 1 << Utils::iota(),
    Show_Asm = 1 << Utils::iota(),
    Test = 1 << Utils::iota(),
}

fn parse_args(args) {
    let flags = 0x0;


    let eat = |exp| {
        let res = args[0];
        if (exp.is_some() && res != exp.unwrap()) {
            panic(f"expected {exp} but got {res}");
        }
        args = args[1:];
        return res;
    };

    let consume_until = |s, until| {
        let buf = "";
        let i = 0;
        while (s[i] != until) {
            buf.append(s[i]);
            i += 1;
        }
        return (buf, s.substr(i, len(s)));
    };

    let handle_help = |_| {
        flags `|= Flag_Type.Help;
        let _ = eat(none);
    };

    let handle_clean = |_| {
        flags `|= Flag_Type.Clean;
        let _ = eat(none);
    };

    let handle_asm = |_| {
        flags `|= Flag_Type.Show_Asm;
        let _ = eat(none);
    };

    let handle_test = |_| {
        panic("test flag is unimplemented");
    };

    while len(args) > 0 {
        with a = args[0] in
        match a {
            "help"  -> { handle_help();  }
            "clean" -> { handle_clean(); }
            "asm"   -> { handle_asm();   }
            "test"  -> { handle_test();  }
            _ -> { panic(f"unknown flag: {a}"); }
        }
    }

    return flags;
}

fn main() {
    let show_asm = false;

    let flags = parse_args(argv()[1:]);

    if (flags `& Flag_Type.Help) { usage(); }
    if (flags `& Flag_Type.Clean) { cleanup(); exit(0); }
    if (flags `& Flag_Type.Show_Asm) { show_asm = true; }

    @const let persist_name = f"{__FILE__}/num_tests";
    let num_tests = persist_lookup(persist_name);
    if (!num_tests) {
        num_tests = some(0);
    } else {
        num_tests = some(int(num_tests.unwrap()));
    }

    cleanup();
    let exes = compile();
    run(exes, show_asm);

    with M = format("(", len(exes), ") from last test run (", num_tests.unwrap(), ")")
    in   if len(exes) > num_tests.unwrap() { info(f"New tests {M}"); }
    else if len(exes) < num_tests.unwrap() { info(f"Removed tests {M}"); }

    persist(persist_name, len(exes));
}

main();
