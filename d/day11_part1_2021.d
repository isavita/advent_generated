
import std.stdio;
import std.file;
import std.string;
import std.conv;
import std.array;
import std.range;
import std.algorithm;
import std.typecons;

void main() {
    auto grid = readText("input.txt").strip.split("\n")
        .map!(line => line.map!(c => c - '0').array).array;

    long totalFlashes = 0;
    int height = cast(int)grid.length;
    int width = cast(int)grid[0].length;

    foreach (step; 0 .. 100) {
        Tuple!(int, int)[] toFlash;

        foreach (y; 0 .. height) {
            foreach (x; 0 .. width) {
                grid[y][x]++;
                if (grid[y][x] > 9) {
                    toFlash ~= tuple(x, y);
                }
            }
        }

        size_t head = 0;
        while (head < toFlash.length) {
            auto p = toFlash[head++];
            int x = p[0], y = p[1];

            if (grid[y][x] == 0) {
                continue;
            }

            grid[y][x] = 0;
            totalFlashes++;

            foreach (dy; -1 .. 2) {
                foreach (dx; -1 .. 2) {
                    if (dx == 0 && dy == 0) continue;

                    int ny = y + dy;
                    int nx = x + dx;

                    if (ny >= 0 && ny < height && nx >= 0 && nx < width && grid[ny][nx] != 0) {
                        grid[ny][nx]++;
                        if (grid[ny][nx] > 9) {
                            toFlash ~= tuple(nx, ny);
                        }
                    }
                }
            }
        }
    }

    writeln(totalFlashes);
}
