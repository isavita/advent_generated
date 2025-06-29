
import std.stdio;
import std.file;
import std.string;
import std.conv;
import std.algorithm;
import std.range;

struct RangeMap {
    long dest, src, len;
}

void main() {
    auto blocks = readText("input.txt").strip.split("\n\n");

    auto seeds = blocks[0].split(":")[1].strip.split.map!(to!long).array;

    auto maps = blocks[1 .. $]
        .map!(block => block.splitLines[1 .. $]
            .filter!(line => !line.empty)
            .map!(line => line.split.to!(long[]))
            .map!(n => RangeMap(n[0], n[1], n[2]))
            .array)
        .array;

    long minLocation = long.max;

    foreach (seed; seeds) {
        long current = seed;
        foreach (mapSet; maps) {
            foreach (r; mapSet) {
                if (current >= r.src && current < r.src + r.len) {
                    current = r.dest + (current - r.src);
                    break;
                }
            }
        }
        minLocation = min(minLocation, current);
    }

    writeln(minLocation);
}
