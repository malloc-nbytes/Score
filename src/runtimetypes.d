module runtimeTypes;

import std.stdio;

import keywords;
import std.conv;

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
        Struct,
}

struct RuntimeType {
        RuntimeTypeBase b;
        RuntimeType* nptr;           // For pointer nesting

        // structs
        string structName;           // The name of the struct
        string[] memberNames;        // Names of struct members
        RuntimeType*[] memberTypes;  // Types of struct members
        size_t[] memberOffsets;      // Offsets of members in memory
        size_t size;                 // Total size (for structs)
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
                default: return Struct;
                }
}

string scrTypeToQbeType(RuntimeType* t) {
        switch (t.b) {
        case RuntimeTypeBase.I8: return "w";
        case RuntimeTypeBase.I16: return "w";
        case RuntimeTypeBase.I32: return "w";
        case RuntimeTypeBase.I64: return "l";
        case RuntimeTypeBase.U8: return "w";
        case RuntimeTypeBase.U16: return "w";
        case RuntimeTypeBase.U32: return "w";
        case RuntimeTypeBase.U64: return "l";
        case RuntimeTypeBase.Usize: return "l";
        case RuntimeTypeBase.Ptr: return "l";
        case RuntimeTypeBase.Void: return "";
        case RuntimeTypeBase.Struct: assert(0);
        default: assert(0);
        }
}

size_t getTypeSize(RuntimeType* t) {
        if (t is null) return 0;
        switch (t.b) {
        case RuntimeTypeBase.I8:
        case RuntimeTypeBase.U8:  return 1;

        case RuntimeTypeBase.I16:
        case RuntimeTypeBase.U16: return 2;

        case RuntimeTypeBase.I32:
        case RuntimeTypeBase.U32: return 4;

        case RuntimeTypeBase.I64:
        case RuntimeTypeBase.U64:
        case RuntimeTypeBase.Usize:
        case RuntimeTypeBase.Ptr: return 8;

        case RuntimeTypeBase.Void: return 0;

        case RuntimeTypeBase.Struct: {
                return t.size;
        } break;

        case RuntimeTypeBase.Unknown:
                assert(0, "Unsupported type size for " ~ t.b.to!string);
        default: assert(0, "Unknown RuntimeTypeBase");
        }
}
