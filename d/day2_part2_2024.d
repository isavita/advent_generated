
import std.stdio;
import std.string;
import std.conv;
import std.algorithm;
import std.array;
import std.math;

bool isSafeSequence(const int[] report) {
    if (report.length < 2) {
        return true;
    }

    if (report[0] == report[1]) {
        return false;
    }
    immutable bool mustBeIncreasing = report[1] > report[0];

    for (size_t i = 0; i < report.length - 1; ++i) {
        immutable current = report[i];
        immutable next = report[i + 1];

        if (mustBeIncreasing ? (next <= current) : (next >= current)) {
            return false;
        }

        immutable diff = abs(next - current);
        if (diff < 1 || diff > 3) {
            return false;
        }
    }

    return true;
}

bool isSafe(int[] report) {
    if (isSafeSequence(report)) {
        return true;
    }

    foreach (i; 0 .. report.length) {
        auto modifiedReport = report[0 .. i] ~ report[i + 1 .. $];
        if (isSafeSequence(modifiedReport)) {
            return true;
        }
    }

    return false;
}

void main() {
    uint safeCount = 0;
    foreach (line; File("input.txt").byLine) {
        auto stripped = line.strip;
        if (stripped.empty) continue;
        
        auto report = stripped.split.map!(to!int).array;
        if (isSafe(report)) {
            safeCount++;
        }
    }
    writeln(safeCount);
}
