
import std.stdio;
import std.string;
import std.conv;
import std.algorithm;
import std.container.dlist;

struct Point {
    int x, y, z;
}

void main() {
    bool[Point] cubes;
    auto minPt = Point(int.max, int.max, int.max);
    auto maxPt = Point(int.min, int.min, int.min);

    foreach (line; File("input.txt").byLine) {
        auto parts = line.strip.split(',');
        if (parts.length < 3) continue;
        auto cube = Point(to!int(parts[0]), to!int(parts[1]), to!int(parts[2]));
        cubes[cube] = true;
        minPt = Point(min(minPt.x, cube.x), min(minPt.y, cube.y), min(minPt.z, cube.z));
        maxPt = Point(max(maxPt.x, cube.x), max(maxPt.y, cube.y), max(maxPt.z, cube.z));
    }

    minPt = Point(minPt.x - 1, minPt.y - 1, minPt.z - 1);
    maxPt = Point(maxPt.x + 1, maxPt.y + 1, maxPt.z + 1);

    long faces = 0;
    auto q = DList!Point(minPt);
    bool[Point] seen;
    seen[minPt] = true;

    immutable Point[] neighbors = [
        Point(-1, 0, 0), Point(1, 0, 0), Point(0, -1, 0),
        Point(0, 1, 0), Point(0, 0, -1), Point(0, 0, 1)
    ];

    while (!q.empty) {
        auto curr = q.front;
        q.removeFront();

        foreach (delta; neighbors) {
            auto next = Point(curr.x + delta.x, curr.y + delta.y, curr.z + delta.z);

            if (next.x < minPt.x || next.y < minPt.y || next.z < minPt.z ||
                next.x > maxPt.x || next.y > maxPt.y || next.z > maxPt.z) {
                continue;
            }

            if (next in cubes) {
                faces++;
            } else if (next !in seen) {
                seen[next] = true;
                q.insertBack(next);
            }
        }
    }

    writeln(faces);
}
