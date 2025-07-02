
import std.stdio;
import std.file;
import std.string;
import std.algorithm;

struct Coord {
    int x, y, z;
}

void main() {
    bool[Coord] activeCubes;
    auto inputLines = readText("input.txt").strip().splitLines();

    foreach (y, line; inputLines) {
        foreach (x, char_; line) {
            if (char_ == '#') {
                activeCubes[Coord(cast(int)x, cast(int)y, 0)] = true;
            }
        }
    }

    foreach (i; 0 .. 6) {
        int[Coord] neighborCounts;
        foreach (coord, _; activeCubes) {
            foreach (dz; -1 .. 2) {
                foreach (dy; -1 .. 2) {
                    foreach (dx; -1 .. 2) {
                        if (dx == 0 && dy == 0 && dz == 0) continue;
                        auto neighbor = Coord(coord.x + dx, coord.y + dy, coord.z + dz);
                        neighborCounts[neighbor]++;
                    }
                }
            }
        }

        bool[Coord] newActiveCubes;
        foreach (coord, count; neighborCounts) {
            if (count == 3 || (count == 2 && (coord in activeCubes))) {
                newActiveCubes[coord] = true;
            }
        }
        activeCubes = newActiveCubes;
    }

    writeln(activeCubes.length);
}
