
import std.stdio, std.file, std.string, std.conv, std.algorithm, std.range, std.math;

void main() {
    long[] ids1, ids2;

    foreach (line; readText("input.txt").strip().splitLines()) {
        auto parts = line.split();
        ids1 ~= to!long(parts[0]);
        ids2 ~= to!long(parts[1]);
    }

    ids1.sort;
    ids2.sort;

    writeln(zip(ids1, ids2).map!(p => abs(p[0] - p[1])).sum);
}
