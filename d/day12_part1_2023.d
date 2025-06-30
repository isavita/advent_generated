
import std.stdio;
import std.file;
import std.string;
import std.conv;
import std.algorithm;
import std.array;
import std.typecons;

struct Row {
    string springs;
    int[] group;
}

alias Cache = long[Tuple!(size_t, size_t, size_t)];

long count(const Row row, size_t iSprings, size_t iGroup, size_t iContiguousDamaged, ref Cache cache) {
    auto key = tuple(iSprings, iGroup, iContiguousDamaged);
    if (key in cache) {
        return cache[key];
    }

    if (iSprings == row.springs.length) {
        if (iGroup == row.group.length && iContiguousDamaged == 0) return 1;
        if (iGroup == row.group.length - 1 && iContiguousDamaged == row.group[iGroup]) return 1;
        return 0;
    }

    long res = 0;
    char c = row.springs[iSprings];

    if (c == '.' || c == '?') {
        if (iContiguousDamaged == 0) {
            res += count(row, iSprings + 1, iGroup, 0, cache);
        } else if (iGroup < row.group.length && iContiguousDamaged == row.group[iGroup]) {
            res += count(row, iSprings + 1, iGroup + 1, 0, cache);
        }
    }

    if (c == '#' || c == '?') {
        if (iGroup < row.group.length && iContiguousDamaged < row.group[iGroup]) {
            res += count(row, iSprings + 1, iGroup, iContiguousDamaged + 1, cache);
        }
    }

    cache[key] = res;
    return res;
}

long countArrangements(const Row row) {
    Cache cache;
    return count(row, 0, 0, 0, cache);
}

void main() {
    auto lines = readText("input.txt").strip.splitLines;
    long total = 0;

    foreach (line; lines) {
        if (line.length == 0) continue;

        auto parts = line.split(' ');
        Row row;
        row.springs = parts[0];
        row.group = parts[1].split(',').map!(to!int).array;
        
        total += countArrangements(row);
    }

    writeln(total);
}
