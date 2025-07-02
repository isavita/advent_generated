
import std.stdio, std.file, std.string, std.conv, std.algorithm, std.array;

long solve(long[] history) {
    if (history.all!(a => a == 0)) {
        return 0;
    }

    auto diffs = new long[history.length - 1];
    foreach (i; 0 .. diffs.length) {
        diffs[i] = history[i + 1] - history[i];
    }

    return history[0] - solve(diffs);
}

void main() {
    long total = 0;
    foreach (line; File("input.txt").byLine) {
        if (line.length == 0) continue;
        auto history = line.split.map!(to!long).array;
        total += solve(history);
    }
    writeln(total);
}
