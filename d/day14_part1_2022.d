
import std.stdio;
import std.string;
import std.conv;
import std.algorithm;
import std.range;

struct Point {
    int x;
    int y;
}

void main() {
    bool[Point] grid;
    int maxY = 0;

    foreach (line; File("input.txt").byLine) {
        auto points = line.strip.split(" -> ")
            .map!(p => p.split(','))
            .map!(coords => Point(coords[0].to!int, coords[1].to!int))
            .array;

        foreach (i; 0 .. points.length - 1) {
            auto p1 = points[i];
            auto p2 = points[i + 1];

            maxY = max(maxY, p1.y, p2.y);

            if (p1.x == p2.x) {
                foreach (y; min(p1.y, p2.y) .. max(p1.y, p2.y) + 1) {
                    grid[Point(p1.x, y)] = true;
                }
            } else {
                foreach (x; min(p1.x, p2.x) .. max(p1.x, p2.x) + 1) {
                    grid[Point(x, p1.y)] = true;
                }
            }
        }
    }

    int sandCount = 0;
    loop: for (;; sandCount++) {
        auto sand = Point(500, 0);
        if (sand in grid) {
            break loop;
        }

        while (true) {
            if (sand.y > maxY) {
                break loop;
            }

            auto down = Point(sand.x, sand.y + 1);
            if (down !in grid) {
                sand = down;
                continue;
            }

            auto downLeft = Point(sand.x - 1, sand.y + 1);
            if (downLeft !in grid) {
                sand = downLeft;
                continue;
            }

            auto downRight = Point(sand.x + 1, sand.y + 1);
            if (downRight !in grid) {
                sand = downRight;
                continue;
            }

            grid[sand] = true;
            break;
        }
    }

    sandCount.writeln;
}
