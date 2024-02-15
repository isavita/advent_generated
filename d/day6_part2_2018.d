
import std.stdio;
import std.algorithm;
import std.array;
import std.conv;
import std.file;
import std.range;
import std.string;
import std.typecons;
import std.math; // Added import for math module

struct Point {
    int x, y;
}

void main() {
    auto input = "input.txt".readText().splitter("\n").array;
    Point[] points;
    foreach (line; input) {
        auto coords = line.split(", ");
        points ~= Point(to!int(coords[0]), to!int(coords[1]));
    }

    int minX = points.map!(p => p.x).minElement;
    int maxX = points.map!(p => p.x).maxElement;
    int minY = points.map!(p => p.y).minElement;
    int maxY = points.map!(p => p.y).maxElement;

    int regionSize = 0;
    foreach (y; minY .. maxY + 1) {
        foreach (x; minX .. maxX + 1) {
            int totalDistance = points.map!(p => abs(x - p.x) + abs(y - p.y)).sum;
            if (totalDistance < 10000) {
                regionSize++;
            }
        }
    }

    writeln(regionSize);
}
