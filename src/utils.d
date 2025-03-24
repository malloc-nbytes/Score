module utils;

import std.stdio;
import core.stdc.stdlib : exit;

void err(const string msg) {
        writeln(msg);
        exit(1);
}
