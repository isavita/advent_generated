
import std.stdio;
import std.file;
import std.string;
import std.array;

struct Point {
    int y;
    int x;
}

void main() {
    auto grid = readText("input.txt").strip.splitLines;
    if (grid.length == 0) {
        writeln(0);
        return;
    }

    immutable int h = cast(int)grid.length;
    immutable int w = cast(int)grid[0].length;

    Point[][dchar] antennas;
    foreach (y, row; grid) {
        foreach (x, c; row) {
            if (c != '.') {
                antennas[c] ~= Point(cast(int)y, cast(int)x);
            }
        }
    }

    bool[Point] antinodes;
    foreach (coords; antennas.values) {
        for (size_t i = 0; i < coords.length; i++) {
            for (size_t j = i + 1; j < coords.length; j++) {
                auto A = coords[i];
                auto B = coords[j];

                auto P1 = Point(2 * A.y - B.y, 2 * A.x - B.x);
                if (P1.y >= 0 && P1.y < h && P1.x >= 0 && P1.x < w) {
                    antinodes[P1] = true;
                }

                auto P2 = Point(2 * B.y - A.y, 2 * B.x - A.x);
                if (P2.y >= 0 && P2.y < h && P2.x >= 0 && P2.x < w) {
                    antinodes[P2] = true;
                }
            }
        }
    }

    writeln(antinodes.length);
}
