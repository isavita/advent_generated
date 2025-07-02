
import std.stdio;
import std.file;
import std.string;
import std.numeric;
import std.math;
import std.algorithm;

long gcd(long a, long b) {
    return std.numeric.gcd(abs(a), abs(b));
}

struct Line {
    long sx, sy, c;
}

void main() {
    auto grid = readText("input.txt").strip().splitLines();
    auto h = grid.length;
    auto w = grid[0].length;

    long[2][][dchar] antennas;
    foreach (y, row; grid) {
        foreach (x, c; row) {
            if (c != '.') {
                antennas[c] ~= [cast(long)y, cast(long)x];
            }
        }
    }

    bool[Line] allLines;
    foreach (coords; antennas) {
        if (coords.length < 2) continue;
        for (size_t i = 0; i < coords.length; ++i) {
            for (size_t j = i + 1; j < coords.length; ++j) {
                auto ay = coords[i][0], ax = coords[i][1];
                auto by = coords[j][0], bx = coords[j][1];

                long dy = by - ay;
                long dx = bx - ax;

                if (dy == 0 && dx == 0) continue;

                long g = gcd(dy, dx);
                long sy = dy / g;
                long sx = dx / g;

                if (sx < 0 || (sx == 0 && sy < 0)) {
                    sx = -sx;
                    sy = -sy;
                }

                long c = sy * ax - sx * ay;
                allLines[Line(sx, sy, c)] = true;
            }
        }
    }

    bool[long[2]] antinodes;
    foreach (line; allLines.keys) {
        auto sx = line.sx, sy = line.sy, c = line.c;

        if (sy == 0) {
            if (c % sx == 0) {
                long y = -c / sx;
                if (y >= 0 && y < h) {
                    foreach (x; 0 .. w) {
                        antinodes[[y, x]] = true;
                    }
                }
            }
        } else if (sx == 0) {
            if (c % sy == 0) {
                long x = c / sy;
                if (x >= 0 && x < w) {
                    foreach (y; 0 .. h) {
                        antinodes[[y, x]] = true;
                    }
                }
            }
        } else {
            foreach (y; 0 .. h) {
                long val = c + sx * y;
                if (val % sy == 0) {
                    long x = val / sy;
                    if (x >= 0 && x < w) {
                        antinodes[[y, x]] = true;
                    }
                }
            }
        }
    }

    writeln(antinodes.length);
}
