module runtimeTypes;

import keywords;

enum RuntimeTypeBase {
        Unknown,
        I8,
        I16,
        I32,
        I64,
        U8,
        U16,
        U32,
        U64,
        Usize,
        Void,
        Ptr,
        Custom,
}

struct RuntimeType {
        RuntimeTypeBase b;
        RuntimeType* nptr;
}

void typeToPtr(RuntimeType* t) {
        RuntimeTypeBase oldBase = t.b;
        RuntimeType* oldNptr = t.nptr;
        t.b = RuntimeTypeBase.Ptr;
        t.nptr = new RuntimeType;
        t.nptr.b = oldBase;
        t.nptr.nptr = null;
}

RuntimeTypeBase getBaseTypeFromStr(const char[] s) {
        with (RuntimeTypeBase)
        switch (s) {
        case TypeKeyword.I8: return I8;
        case TypeKeyword.I16: return I16;
        case TypeKeyword.I32: return I32;
        case TypeKeyword.I64: return I64;
        case TypeKeyword.U8: return U8;
        case TypeKeyword.U16: return U16;
        case TypeKeyword.U32: return U32;
        case TypeKeyword.U64: return U64;
        case TypeKeyword.Usize: return Usize;
        case TypeKeyword.Void: return Void;
        default: return Custom;
        }
}
