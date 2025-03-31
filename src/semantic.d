module semantic;

class Symbol {
        string name;     // "p", "f"
        Type type;       // StructType, FunctionType, etc.
        Scope scope_;    // Reference to owning scope
        int address;     // Memory offset or register (set later)
        this(string name, Type type, Scope scope) {
                this.name = name;
                this.type = type;
                this.scope_ = scope_;
                this.address = -1;  // Unassigned until codegen
        }
}

class Scope {
        Scope parent;            // Null for global scope
        Symbol[string] symbols;  // Hash map of name -> Symbol
        this(Scope parent) {
                this.parent = parent;
                this.symbols = new Symbol[string];
        }

        void addSymbol(Symbol sym) {
                symbols[sym.name] = sym;
        }

        Symbol lookup(string name) {
                if (name in symbols) return symbols[name];
                if (parent) return parent.lookup(name);
                return null;  // Not found
        }
}
