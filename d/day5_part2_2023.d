
import std.stdio;
import std.file;
import std.string;
import std.conv;
import std.array;
import std.algorithm;
import std.range;

struct Range {
    long destStart;
    long srcStart;
    long length;
}

struct SeedRange {
    long start;
    long length;
}

long reverseConvertNumber(long number, Range[] ranges) {
    foreach (r; ranges) {
        if (number >= r.destStart && number < r.destStart + r.length) {
            return r.srcStart + (number - r.destStart);
        }
    }
    return number;
}

bool isInSeedRanges(long number, SeedRange[] seedRanges) {
    foreach (r; seedRanges) {
        if (number >= r.start && number < r.start + r.length) {
            return true;
        }
    }
    return false;
}

void main() {
    auto blocks = readText("input.txt").strip().split("\n\n");

    SeedRange[] seedRanges;
    auto seedInfo = blocks[0].split(':')[1].strip().split();
    for (size_t i = 0; i < seedInfo.length; i += 2) {
        seedRanges ~= SeedRange(seedInfo[i].to!long, seedInfo[i+1].to!long);
    }

    Range[][] maps;
    foreach (block; blocks[1..$]) {
        Range[] currentRanges;
        foreach (line; block.split('\n')[1..$]) {
            if (line.strip().empty) continue;
            auto numbers = line.split().map!(to!long).array;
            currentRanges ~= Range(numbers[0], numbers[1], numbers[2]);
        }
        maps ~= currentRanges;
    }

    long location = 0;
    while (true) {
        long seed = location;
        foreach_reverse (m; maps) {
            seed = reverseConvertNumber(seed, m);
        }

        if (isInSeedRanges(seed, seedRanges)) {
            writeln(location);
            break;
        }
        location++;
    }
}
