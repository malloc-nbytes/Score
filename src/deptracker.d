module depTracker;

import token;
import grammar;
import visitor;

class DepTbl {
        string file;
        string[] deps;

        this(string file) {
                this.file = file;
                this.deps = [];
        }

        bool hasDep(const ref string d) {
                for (size_t i = 0; i < this.deps.length; ++i) {
                        if (d == this.deps[i]) {
                                return true;
                        }
                }
                return false;
        }
}

DepTbl determineDeps(Program* p, string file) {
        DepTbl tbl = new DepTbl(file);
        for (size_t i = 0; i < p.stmts.length; ++i) {
                if (p.stmts[i].ty == StmtType.Import) {
                        StmtImport imp = cast(StmtImport)p.stmts[i];
                        tbl.deps ~= imp.id.lx.idup;
                }
        }
        return tbl;
}

