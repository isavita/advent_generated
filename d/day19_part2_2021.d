
import std.stdio;
import std.file;
import std.string;
import std.conv;
import std.array;
import std.algorithm;
import std.math;
import core.stdc.stdio;

struct Coord {
    int x, y, z;

    Coord opBinary(string op)(Coord rhs) const if (op == "+" || op == "-") {
        mixin("return Coord(x " ~ op ~ " rhs.x, y " ~ op ~ " rhs.y, z " ~ op ~ " rhs.z);");
    }

    hash_t toHash() const @safe {
        hash_t h = x;
        h = (h * 31) + y;
        h = (h * 31) + z;
        return h;
    }

    bool opEquals(ref const Coord other) const {
        return x == other.x && y == other.y && z == other.z;
    }
}

struct Scanner {
    Coord pos;
    Coord[][] rotations;
    Coord[] absoluteBeacons;
    bool[Coord] absoluteBeaconsSet;
}

Coord[][] generateRotations(Coord[] beacons) {
    alias RotFunc = Coord function(Coord);
    immutable RotFunc[] rots = [
        (c) => Coord( c.x,  c.y,  c.z), (c) => Coord( c.x, -c.z,  c.y),
        (c) => Coord( c.x, -c.y, -c.z), (c) => Coord( c.x,  c.z, -c.y),
        (c) => Coord(-c.x,  c.y, -c.z), (c) => Coord(-c.x, -c.z, -c.y),
        (c) => Coord(-c.x, -c.y,  c.z), (c) => Coord(-c.x,  c.z,  c.y),
        (c) => Coord( c.y, -c.x,  c.z), (c) => Coord( c.y, -c.z, -c.x),
        (c) => Coord( c.y,  c.x, -c.z), (c) => Coord( c.y,  c.z,  c.x),
        (c) => Coord(-c.y,  c.x,  c.z), (c) => Coord(-c.y, -c.z,  c.x),
        (c) => Coord(-c.y, -c.x, -c.z), (c) => Coord(-c.y,  c.z, -c.x),
        (c) => Coord( c.z,  c.y, -c.x), (c) => Coord( c.z,  c.x,  c.y),
        (c) => Coord( c.z, -c.y,  c.x), (c) => Coord( c.z, -c.x, -c.y),
        (c) => Coord(-c.z,  c.y,  c.x), (c) => Coord(-c.z, -c.x,  c.y),
        (c) => Coord(-c.z, -c.y, -c.x), (c) => Coord(-c.z,  c.x, -c.y),
    ];

    return rots.map!(rot => beacons.map!(b => rot(b)).array).array;
}

Scanner[] parseInput(string input) {
    Scanner[] scanners;
    foreach (block; input.strip.split("\n\n")) {
        auto lines = block.splitLines();
        Coord[] beacons;
        foreach (line; lines[1 .. $]) {
            auto parts = line.split(',');
            beacons ~= Coord(parts[0].to!int, parts[1].to!int, parts[2].to!int);
        }
        scanners ~= Scanner(Coord(0,0,0), generateRotations(beacons));
    }
    return scanners;
}

bool tryAlignScanner(ref Scanner undet, const Scanner[] settled) {
    foreach (const ref set; settled) {
        foreach (const rotatedBeacons; undet.rotations) {
            foreach (const settledBeacon; set.absoluteBeacons) {
                foreach (const rotatedBeacon; rotatedBeacons) {
                    auto scannerPos = settledBeacon - rotatedBeacon;
                    int count = 0;
                    foreach (const otherRotatedBeacon; rotatedBeacons) {
                        if ((scannerPos + otherRotatedBeacon) in set.absoluteBeaconsSet) {
                            count++;
                        }
                    }

                    if (count >= 12) {
                        undet.pos = scannerPos;
                        undet.absoluteBeacons = rotatedBeacons.map!(b => scannerPos + b).array;
                        foreach (const ab; undet.absoluteBeacons) {
                            undet.absoluteBeaconsSet[ab] = true;
                        }
                        return true;
                    }
                }
            }
        }
    }
    return false;
}

int solve(Scanner[] scanners) {
    auto settled = [scanners[0]];
    auto undetermined = scanners[1 .. $].dup;

    settled[0].absoluteBeacons = settled[0].rotations[0];
    foreach (b; settled[0].absoluteBeacons) {
        settled[0].absoluteBeaconsSet[b] = true;
    }

    while (!undetermined.empty) {
        for (int i = 0; i < undetermined.length; ++i) {
            if (tryAlignScanner(undetermined[i], settled)) {
                settled ~= undetermined[i];
                undetermined = undetermined.remove(i);
                break;
            }
        }
    }

    int maxDist = 0;
    for (int i = 0; i < settled.length; ++i) {
        for (int j = i + 1; j < settled.length; ++j) {
            int dist = abs(settled[i].pos.x - settled[j].pos.x) +
                       abs(settled[i].pos.y - settled[j].pos.y) +
                       abs(settled[i].pos.z - settled[j].pos.z);
            maxDist = max(maxDist, dist);
        }
    }
    return maxDist;
}

void main() {
    auto scanners = parseInput(readText("input.txt"));
    writeln(solve(scanners));
}
