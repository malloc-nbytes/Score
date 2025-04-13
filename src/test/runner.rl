module Runner

import "std/system.rl"; as sys
import "std/io.rl"; as io

### This file is the test runner for the Score tests.

fn get_test_files() {
    sys::ls(".").filter(|k| {
        with parts = sys::name_and_ext(k) in
        return parts[1].is_some() && parts[1].unwrap() == "scr";
    });
}

let files = get_test_files();

foreach f in files {
}


