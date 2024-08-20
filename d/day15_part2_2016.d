import std.stdio;
import std.file;
import std.conv;
import std.string;

struct Disc {
    int positions;
    int start;
}

Disc[] parseInput(string filename) {
    Disc[] discs;
    foreach (line; File(filename).byLine()) {
        auto parts = line.split(" ");
        int positions = to!int(parts[3]);
        int start = to!int(parts[11][0..$-1]);
        discs ~= Disc(positions, start);
    }
    return discs;
}

bool fallsThrough(Disc[] discs, int time) {
    foreach (i, disc; discs) {
        if ((disc.start + time + i + 1) % disc.positions != 0) {
            return false;
        }
    }
    return true;
}

void main() {
    auto discs = parseInput("input.txt");

    int time = 0;
    while (!fallsThrough(discs, time)) {
        time++;
    }
    writeln("Part One: ", time);

    discs ~= Disc(11, 0);

    time = 0;
    while (!fallsThrough(discs, time)) {
        time++;
    }
    writeln("Part Two: ", time);
}