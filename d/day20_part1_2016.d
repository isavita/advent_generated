import std.stdio;
import std.file;
import std.conv;
import std.algorithm;
import std.array;
import std.string;

struct Range {
    uint start;
    uint end;
}

void main() {
    auto file = File("input.txt", "r");
    Range[] blockedRanges;

    foreach (line; file.byLine()) {
        auto parts = line.split("-");
        uint start = to!uint(parts[0]);
        uint end = to!uint(parts[1]);
        blockedRanges ~= Range(start, end);
    }

    blockedRanges.sort!((a, b) => a.start < b.start || (a.start == b.start && a.end < b.end));

    uint lowestAllowedIP = 0;
    foreach (range; blockedRanges) {
        if (lowestAllowedIP < range.start) {
            break;
        }
        if (lowestAllowedIP <= range.end) {
            lowestAllowedIP = range.end + 1;
        }
    }

    writeln(lowestAllowedIP);
}