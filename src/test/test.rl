#!/usr/local/bin/earl

module Test

import "std/system.rl"; as sys

set_flag("-x");

fn setup_bld() {
    let old_bins = sys::ls(".")
        .map(|f| {
            let parts = f.split(".").filter(!= "");
            if len(parts) == 1 || parts[1] == "o" { return f; }
            return "";
        })
        .filter(!= "");

    foreach b in old_bins {
        $f"rm {b}";
    }

    ```
    cd ../../
    rm -r ./build > /dev/null 2>&1
    mkdir build > /dev/null 2>&1
    cd build
    cmake -S .. -B .
    make clean
    make -j$(nproc)
    ```;
}

fn run() {
    let tests = sys::ls(".").filter(!= argv()[0]);
    foreach test in tests {
        println(f"Running {test}...");
        $f"../../build/scr {test}";
        $f"./scr_output";
    }
}

println("--- RUNNING TESTS ---");
setup_bld();
run();
