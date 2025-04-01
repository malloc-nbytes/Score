module ir;

enum OpCode {
        LoadIm,
        Load,
        Store,
        Add,
        Sub,
        Mul,
        Div,
        Eq,
        Call,
        Return,
        Label,
        Param,
        Alloc,
        Jump,
        JumpIf,
        JumpIfNot,
        Push,
        Pop,
        Mov,
        Lea,
}

struct Instruction {
        OpCode op;
        string result; // Destination e.g., t1
        string[] operands; // Sources e.g., [t2, t3]
        this(OpCode op, string result, string[] operands) {
                this.op = op;
                this.result = result;
                this.operands = operands;
        }
}

struct ProgramIR {
        Instruction[] instructions;
        void add(Instruction instr) {
                this.instructions ~= instr;
        }
}
