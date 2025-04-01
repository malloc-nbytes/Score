module codegen;

import std.conv;
import std.stdio;
import std.string;

import ir;

void generateAssembly(ProgramIR programIR, string outputFile) {
        File f = File(outputFile, "w");

        // Header
        f.writeln("section .text");
        f.writeln("global main");
        f.writeln("");

        // Register mapping for temporaries (simplified)
        string[int] regMap;  // Maps tmpCount (e.g., t0 -> 0) to registers
        string[] registers = ["rbx", "rcx", "rdx", "rsi", "rdi", "r8", "r9", "r10", "r11"];

        foreach (instr; programIR.instructions) {
                switch (instr.op) {
                case OpCode.Label: {
                        f.writefln("%s:", instr.result);
                } break;

                case OpCode.Push: {
                        f.writefln("    push %s", instr.result);
                } break;

                case OpCode.Pop: {
                        f.writefln("    pop %s", instr.result);
                } break;

                case OpCode.Mov: {
                        f.writefln("    mov %s, %s", instr.result, instr.operands[0]);
                } break;

                case OpCode.Lea: {
                        // Map temporary to a register
                        int tmpNum = to!int(instr.result[1..$]);  // e.g., t0 -> 0
                        string reg = tmpNum < registers.length ? registers[tmpNum] : "rax";  // Fallback to rax
                        regMap[tmpNum] = reg;
                        f.writefln("    lea %s, %s", reg, instr.operands[0]);  // e.g., lea rbx, [rbp - 4]
                } break;

                case OpCode.LoadIm: {
                        int tmpNum = to!int(instr.result[1..$]);
                        string reg = tmpNum < registers.length ? registers[tmpNum] : "rax";
                        regMap[tmpNum] = reg;
                        f.writefln("    mov %s, %s", reg, instr.operands[0]);  // e.g., mov rdx, 3
                } break;

                case OpCode.Store: {
                        // Handle stack offsets and register moves
                        string target = instr.result;
                        string sourceTmp = instr.operands[0];
                        int srcNum = to!int(sourceTmp[1..$]);
                        string srcReg = srcNum in regMap ? regMap[srcNum] : "rax";

                        if (target.startsWith("[rbp -")) {
                                // Direct stack store
                                f.writefln("    mov %s, %s", target, srcReg);
                        } else {
                                // Store to an address in a register (e.g., t0 + 0)
                                int targetNum = to!int(target[1..indexOf(target, " +")]);
                                string targetReg = targetNum in regMap ? regMap[targetNum] : "rax";
                                string offset = target[indexOf(target, " + ") + 3 .. $];
                                f.writefln("    mov [%s + %s], %s", targetReg, offset, srcReg);
                        }
                } break;

                case OpCode.Return: {
                        if (instr.operands.length > 0) {
                                int retNum = to!int(instr.operands[0][1..$]);
                                string retReg = retNum in regMap ? regMap[retNum] : "rax";
                                f.writefln("    mov rax, %s", retReg);  // Return value in rax
                        }
                        f.writeln("    mov rsp, rbp");
                        f.writeln("    pop rbp");
                        f.writeln("    ret");
                } break;

                case OpCode.Alloc: {
                        // Deprecated with stack allocation; skip or error
                        f.writefln("    ; Skipping deprecated Alloc: %s", instr.result);
                } break;

                case Opcode.Call: {
                        assert(0);
                } break;

                default:
                        f.writefln("    ; TODO: %s", instr.op);
                        break;
                }
        }

        // No .bss section needed; all variables are stack-allocated
}

