module assemble;

import std.process;
import std.stdio;
import core.stdc.stdlib : exit;

void ld(string outputName) {
        auto result = execute(["ld", "-o", outputName, outputName~".o"]);
        if (result.status != 0) {
                writeln("Could not link: ", outputName, ": ", result.output);
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
                writeln("Could not assemble: ", outputName, ": ", result.output);
                exit(1);
        }
}
