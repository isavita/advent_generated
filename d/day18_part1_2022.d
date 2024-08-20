import std.stdio;
import std.file;
import std.conv;
import std.array;
import std.string;
import std.algorithm; // Added import for map

struct Point {
    int x, y, z;
}

void main() {
    auto file = File("input.txt", "r");
    auto lines = file.byLine.map!(a => a.idup).array;

    Point[] points;
    foreach (line; lines) {
        auto coords = line.split(",");
        points ~= Point(to!int(coords[0]), to!int(coords[1]), to!int(coords[2]));
    }

    int surfaceArea = 0;
    foreach (i, p1; points) {
        int exposedSides = 6;
        foreach (j, p2; points) {
            if (i != j) {
                if ((p1.x == p2.x && p1.y == p2.y && (p1.z == p2.z + 1 || p1.z == p2.z - 1)) ||
                    (p1.x == p2.x && p1.z == p2.z && (p1.y == p2.y + 1 || p1.y == p2.y - 1)) ||
                    (p1.y == p2.y && p1.z == p2.z && (p1.x == p2.x + 1 || p1.x == p2.x - 1))) {
                    exposedSides--;
                }
            }
        }
        surfaceArea += exposedSides;
    }

    writeln(surfaceArea);
}