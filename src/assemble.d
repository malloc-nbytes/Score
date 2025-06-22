module assemble;

import std.process;
import std.stdio;
import core.stdc.stdlib : exit;
import std.typecons;

void ld(string outputName, bool lc) {
        int status;
        string output;
        if (lc) {
                auto result = execute(["ld", "-dynamic-linker", "/lib64/ld-linux-x86-64.so.2", "-lc", "-o", outputName, outputName~".o"]);
                status = result.status;
                output = result.output;
        } else {
                auto result = execute(["ld", "-o", outputName, outputName~".o"]);
                status = result.status;
                output = result.output;
        }
        if (status != 0) {
                writeln("Could not link: ", outputName, ": ", output);
                exit(1);
        }
}

void nasm_assemble(string outputName) {
        auto result = execute(["nasm",
                               "-f", "elf64",
                               "-g", "-F", "dwarf",
                               outputName~".asm",
                               "-o", outputName~".o"]);
        if (result.status != 0) {
                writeln("NASM: Could not assemble: ", outputName, ": ", result.output);
                exit(1);
        }
}
