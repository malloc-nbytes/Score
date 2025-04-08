module codegen;

import std.conv;
import std.stdio;
import std.string;

import ir;

void generateAssembly(ProgramIR programIR, string outputFile) {
        File f = File(outputFile, "w");

        f.writeln("section .note.GNU-stack");
        f.writeln("section .text");
        f.writeln("global main");
        f.writeln("extern printf");
        f.writeln("");

        string[int] regMap; // Maps tmp number (e.g., 5 for t5) to register (e.g., "r8")
        string[] registers = ["rbx", "rcx", "rdx", "rsi", "rdi", "r8", "r9", "r10", "r11"];
        string[] argRegisters = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

        string[string] stringLiterals;
        string[string] labelMap;
        int strCounter = 0;

        foreach (instr; programIR.instructions) {
                switch (instr.op) {
                case OpCode.Label:
                        f.writefln("%s:", instr.result);
                        break;

                case OpCode.Push:
                        f.writefln("    push %s", instr.result);
                        break;

                case OpCode.Pop:
                        f.writefln("    pop %s", instr.result);
                        break;

                case OpCode.Mov:
                        f.writefln("    mov %s, %s", instr.result, instr.operands[0]);
                        break;

                case OpCode.Lea:
                        int tmpNum = to!int(instr.result[1..$]);
                        string reg = tmpNum < registers.length ? registers[tmpNum] : "rax";
                        regMap[tmpNum] = reg;
                        string operand = instr.operands[0];
                        if (operand in labelMap) {
                                operand = labelMap[operand];
                        }
                        f.writefln("    lea %s, %s", reg, operand);
                        break;

                case OpCode.Load:
                        int tmpNum = to!int(instr.result[1..$]);
                        string reg = tmpNum < registers.length ? registers[tmpNum] : "rax";
                        regMap[tmpNum] = reg;
                        string operand = instr.operands[0];
                        // Handle cases like "t5 + 0"
                        if (operand.indexOf(" + ") != -1) {
                                string[] parts = operand.split(" + ");
                                int baseNum = to!int(parts[0][1..$]); // e.g., 5 from "t5"
                                string baseReg = baseNum in regMap ? regMap[baseNum] : "rax";
                                string offset = parts[1];
                                f.writefln("    mov %s, [%s + %s]", reg, baseReg, offset);
                        } else {
                                f.writefln("    mov %s, %s", reg, operand); // Direct load (e.g., from memory or reg)
                        }
                        break;

                case OpCode.LoadIm:
                        int tmpNum = to!int(instr.result[1..$]);
                        string reg = tmpNum < registers.length ? registers[tmpNum] : "rax";
                        regMap[tmpNum] = reg;
                        f.writefln("    mov %s, %s", reg, instr.operands[0]);
                        break;

                case OpCode.Store:
                        string target = instr.result;
                        string sourceTmp = instr.operands[0];
                        int srcNum = to!int(sourceTmp[1..$]);
                        string srcReg = srcNum in regMap ? regMap[srcNum] : "rax";

                        if (target.startsWith("[rbp -")) {
                                f.writefln("    mov %s, %s", target, srcReg);
                        } else if (target.indexOf(" + ") != -1) {
                                string[] parts = target.split(" + ");
                                int targetNum = to!int(parts[0][1..$]);
                                string targetReg = targetNum in regMap ? regMap[targetNum] : "rax";
                                string offset = parts[1];
                                f.writefln("    mov [%s + %s], %s", targetReg, offset, srcReg);
                        } else {
                                f.writefln("    mov %s, %s", target, srcReg); // Direct store
                        }
                        break;

                case OpCode.Return:
                        if (instr.operands.length > 0) {
                                int retNum = to!int(instr.operands[0][1..$]);
                                string retReg = retNum in regMap ? regMap[retNum] : "rax";
                                f.writefln("    mov rax, %s", retReg);
                        }
                        f.writeln("    mov rsp, rbp");
                        f.writeln("    pop rbp");
                        f.writeln("    ret");
                        break;

                case OpCode.Alloc:
                        f.writefln("    ; Skipping deprecated Alloc: %s", instr.result);
                        break;

                case OpCode.Call:
                        string funcName = instr.operands[0];
                        string[] args = instr.operands[1..$];

                        // foreach (i; 0 .. registers.length) {
                        //         if (cast(int)i in regMap) {
                        //                 f.writefln("    push %s", registers[i]);
                        //         }
                        // }

                        foreach (i, arg; args) {
                                int argNum = to!int(arg[1..$]);
                                string argReg = argNum in regMap ? regMap[argNum] : "rax";

                                if (i < argRegisters.length) {
                                        f.writefln("    mov %s, %s", argRegisters[i], argReg);
                                } else {
                                        f.writefln("    push %s", argReg);
                                }
                        }

                        size_t stackArgs = args.length > argRegisters.length ? args.length - argRegisters.length : 0;
                        bool misaligned = ((stackArgs + 1) % 2 == 0);  // +1 for rbp
                        if (misaligned) {
                                f.writeln("    sub rsp, 8");
                        }

                        f.writefln("    call %s", funcName);

                        if (stackArgs > 0 || misaligned) {
                                size_t cleanup = stackArgs * 8 + (misaligned ? 8 : 0);
                                f.writefln("    add rsp, %d", cleanup);
                        }

                        // foreach_reverse (i; 0 .. registers.length) {
                        //         if (cast(int)i in regMap) {
                        //                 f.writefln("    pop %s", registers[i]);
                        //         }
                        // }

                        int retNum = to!int(instr.result[1..$]);
                        string retReg = retNum < registers.length ? registers[retNum] : "rax";
                        regMap[retNum] = retReg;
                        f.writefln("    mov %s, rax", retReg);
                        break;

                case OpCode.StrLit:
                        string strContent = instr.operands[0];
                        string origLabel = instr.result;
                        if (strContent !in stringLiterals) {
                                string dedupLabel = format("str%d", strCounter++);
                                stringLiterals[strContent] = dedupLabel;
                        }
                        labelMap[origLabel] = stringLiterals[strContent];
                        break;

                default:
                        f.writefln("    ; TODO: %s", instr.op);
                        break;
                }
        }

        if (stringLiterals.length > 0) {
                f.writeln("");
                f.writeln("section .rodata");
                foreach (strContent, label; stringLiterals) {
                        string escapedStr = strContent.replace("\\n", "\", 10, \"");
                        f.writefln("%s: db \"%s\", 0", label, escapedStr);
                }
        }
}

