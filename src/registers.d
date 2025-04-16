module registers;

import std.stdio;

//=====================================================================================REGISTERS
// calling order: rdi, rsi, rdx, rcx, r8, r9.

// https://math.hws.edu/eck/cs220/f22/registers.html

// 64 bit
// rax, rbx, rcx, rdx, rsi, rdi, r8, r9, r10, r11, r12, r13, r14, r15

// 32 bit
// eax, ebx, ecx, edx, esi, edi, r8d, r9d, r10d, r11d, r12d, r13d, r14d, r15d

// 16 bit
// ax, bx, cx, dx, si, di, r8w, r9w, r10w, r11w, r12w, r13w, r14w, r15w

// 8 bit
// (ah,al), (bh,bl), (ch,cl), (dh,dl), r8b, r9b, r10b, r11b, r12b, r13b, r14b, r15b
//=====================================================================================END REGISTERS

struct Register {
        Register* next;
        Register* down, up;
        bool inUse;
        string name;

        this(string name, Register* next = null, Register* up = null, Register* down = null) {
                this.next = next;
                this.up = up;
                this.down = down;
                this.inUse = false;
                this.name = name;
        }

        bool regInUse() {
                if (inUse) { return true; }
                Register* it = this.up;
                while (it) {
                        if (it.inUse) { return true; }
                        it = it.up;
                }
                it = this.down;
                while (it) {
                        if (it.inUse) { return true; }
                        it = it.down;
                }
                return false;
        }

        void append(Register** r) {
                Register* it = this.next;
                Register* p = &this;
                while (it) {
                        p = it;
                        it = it.next;
                }
                it = *r;
                p.next = it;
        }

        void appendDown(Register** r) {
                Register* it = this.down;
                Register* p = &this;
                while (it) {
                        p = it;
                        it = it.down;
                }
                it = *r;
                p.down = it;
                if (it) {
                        it.up = p;
                }
        }

        void dump() {
                writeln("DUMPING: ", name);
                write("NEXT: ");
                Register* it = this.next;
                while (it) {
                        write(it.name);
                        it = it.next;
                        if (it) { write(' '); }
                }
                write("\nDOWN: ");
                it = this.down;
                while (it) {
                        write(it.name);
                        it = it.down;
                        if (it) { write(' '); }
                }
                write("\nUP: ");
                it = this.up;
                while (it) {
                        write(it.name);
                        it = it.up;
                        if (it) { write(' '); }
                }
                writeln();
        }
}

void buildRegisters(Register** r10, Register** rdi, Register** rax) {
        *r10 = new Register("r10");
        Register* r11 = new Register("r11");
        Register* rbx = new Register("rbx");
        Register* r12 = new Register("r12");
        Register* r13 = new Register("r13");
        Register* r14 = new Register("r14");
        Register* r15 = new Register("r15");

        Register* r10d = new Register("r10d");
        Register* r11d = new Register("r11d");
        Register* ebx = new Register("ebx");
        Register* r12d = new Register("r12d");
        Register* r13d = new Register("r13d");
        Register* r14d = new Register("r14d");
        Register* r15d = new Register("r15d");

        Register* r10w = new Register("r10w");
        Register* r11w = new Register("r11w");
        Register* bx = new Register("bx");
        Register* r12w = new Register("r12w");
        Register* r13w = new Register("r13w");
        Register* r14w = new Register("r14w");
        Register* r15w = new Register("r15w");

        Register* r10b = new Register("r10b");
        Register* r11b = new Register("r11b");
        Register* bl = new Register("bl");
        Register* r12b = new Register("r12b");
        Register* r13b = new Register("r13b");
        Register* r14b = new Register("r14b");
        Register* r15b = new Register("r15b");

        (*r10).append(&r11);
        (*r10).append(&rbx);
        (*r10).append(&r12);
        (*r10).append(&r13);
        (*r10).append(&r14);
        (*r10).append(&r15);

        r10d.append(&r11d);
        r10d.append(&ebx);
        r10d.append(&r12d);
        r10d.append(&r13d);
        r10d.append(&r14d);
        r10d.append(&r15d);

        r10w.append(&r11w);
        r10w.append(&bx);
        r10w.append(&r12w);
        r10w.append(&r13w);
        r10w.append(&r14w);
        r10w.append(&r15w);

        r10b.append(&r11b);
        r10b.append(&bl);
        r10b.append(&r12b);
        r10b.append(&r13b);
        r10b.append(&r14b);
        r10b.append(&r15b);

        (*r10).appendDown(&r10d);
        (*r10).appendDown(&r10w);
        (*r10).appendDown(&r10b);

        r11.appendDown(&r11d);
        r11.appendDown(&r11w);
        r11.appendDown(&r11b);

        rbx.appendDown(&ebx);
        rbx.appendDown(&bx);
        rbx.appendDown(&bl);

        r12.appendDown(&r12d);
        r12.appendDown(&r12w);
        r12.appendDown(&r12b);

        r13.appendDown(&r13d);
        r13.appendDown(&r13w);
        r13.appendDown(&r13b);

        r14.appendDown(&r14d);
        r14.appendDown(&r14w);
        r14.appendDown(&r14b);

        r15.appendDown(&r15d);
        r15.appendDown(&r15w);
        r15.appendDown(&r15b);

        *rdi = new Register("rdi");
        Register* rsi = new Register("rsi");
        Register* rdx = new Register("rdx");
        Register* rcx = new Register("rcx");
        Register* r8 = new Register("r8");
        Register* r9 = new Register("r9");

        Register* edi = new Register("edi");
        Register* esi = new Register("esi");
        Register* edx = new Register("edx");
        Register* ecx = new Register("ecx");
        Register* r8d = new Register("r8d");
        Register* r9d = new Register("r9d");

        Register* di = new Register("di");
        Register* si = new Register("si");
        Register* dx = new Register("dx");
        Register* cx = new Register("cx");
        Register* r8w = new Register("r8w");
        Register* r9w = new Register("r9w");

        Register* dil = new Register("dil");
        Register* sil = new Register("sil");
        Register* dl = new Register("dl");
        Register* cl = new Register("cl");
        Register* r8b = new Register("r8b");
        Register* r9b = new Register("r9b");

        (*rdi).append(&rsi);
        (*rdi).append(&rdx);
        (*rdi).append(&rcx);
        (*rdi).append(&r8);
        (*rdi).append(&r9);

        edi.append(&esi);
        edi.append(&edx);
        edi.append(&ecx);
        edi.append(&r8d);
        edi.append(&r9d);

        di.append(&si);
        di.append(&dx);
        di.append(&cx);
        di.append(&r8w);
        di.append(&r9w);

        dil.append(&sil);
        dil.append(&dl);
        dil.append(&cl);
        dil.append(&r8b);
        dil.append(&r9b);

        (*rdi).appendDown(&edi);
        (*rdi).appendDown(&di);
        (*rdi).appendDown(&dil);

        rsi.appendDown(&esi);
        rsi.appendDown(&si);
        rsi.appendDown(&sil);

        rdx.appendDown(&edx);
        rdx.appendDown(&dx);
        rdx.appendDown(&dl);

        rcx.appendDown(&ecx);
        rcx.appendDown(&cx);
        rcx.appendDown(&cl);

        r8.appendDown(&r8d);
        r8.appendDown(&r8w);
        r8.appendDown(&r8b);

        r9.appendDown(&r9d);
        r9.appendDown(&r9w);
        r9.appendDown(&r9b);

        *rax = new Register("rax");
        Register* eax = new Register("eax");
        Register* ax = new Register("ax");
        Register* al = new Register("al");

        (*rax).appendDown(&eax);
        (*rax).appendDown(&ax);
        (*rax).appendDown(&al);
}
