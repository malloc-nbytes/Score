module gatherIdentifiers;

import grammar;
import visitor;

struct IdentGatherer {
        ExprProcCall[] procs;
        StmtStruct[] structs;
        StmtLet[] lets;
}

private Visitor createGatherer() {
        Visitor v;
        v.context = null;
        assert(0);
}

void getIdents(Program* p) {
        Visitor v = createGatherer();
        assert(0);
}
