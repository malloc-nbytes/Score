module types;

enum TypeKind {
        Primitive,
        Ptr,
        Struct,
        Proc,
}

class Type {
        TypeKind kind;
        size_t size;
        this(TypeKind kind, size_t size) {
                this.kind = kind;
                this.size = size;
        }
}

class Ptr : Type {
        Type to;
        this(Type to) {
                super(TypeKind.Ptr, 8);
                this.to = to;
        }
}

class PrimitiveType : Type {
        string name;
        this(string name, size_t size) {
                super(TypeKind.Primitive, size);
                this.name = name;
        }
}

class Field {
        string name; // 'x', 'y', ...
        Type type;
        size_t offset;
        this(string name, Type type) {
                this.name = name;
                this.type = type;
                this.offset = offset;
        }
}

class StructType : Type {
        string name;
        Field[] fields;
        this(string name, Field[] fields) {
                this.name = name;
                this.fields = fields;
                super(TypeKind.Struct, this.computeSize());
        }

        size_t computeSize() {
                size_t sz;
                size_t offset = 0;
                foreach (field; fields) {
                        field.offset = offset;
                        offset += field.type.size;
                        sz += field.type.size;
                }
                return sz;
        }
}

class ProcType : Type {
        Type returnType;
        Type[] paramTypes;
        this(Type returnType, Type[] paramTypes, size_t size) {
                super(TypeKind.Proc, size);
                this.returnType = returnType;
                this.paramTypes = paramTypes;
        }
}
