module utils;

import std.stdio;
import core.stdc.stdlib : exit;

void err(const string msg) {
        writeln("[Error]: ", msg);
        exit(1);
}
