
import std.stdio;
import std.file;
import std.range;

void main() {
    long total = 0;
    foreach (line; File("input.txt").byLine) {
        if (line.empty) continue;

        auto mid = line.length / 2;
        bool[53] seen;

        foreach (c; line[0 .. mid]) {
            int p = (c >= 'a') ? (c - 'a' + 1) : (c - 'A' + 27);
            seen[p] = true;
        }

        foreach (c; line[mid .. $]) {
            int p = (c >= 'a') ? (c - 'a' + 1) : (c - 'A' + 27);
            if (seen[p]) {
                total += p;
                seen[p] = false;
            }
        }
    }
    writeln(total);
}
