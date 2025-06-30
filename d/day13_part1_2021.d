
import std.stdio;
import std.file;
import std.string;
import std.conv;
import std.algorithm;

struct Point {
    int x;
    int y;
}

void main() {
    auto sections = readText("input.txt").strip().split("\n\n");

    bool[Point] points;
    foreach (line; sections[0].splitLines()) {
        auto coords = line.split(',');
        points[Point(coords[0].to!int, coords[1].to!int)] = true;
    }

    auto instruction = sections[1].splitLines()[0][11 .. $];
    char axis = instruction[0];
    int value = instruction[2 .. $].to!int;

    bool[Point] newPoints;
    foreach (p; points.keys) {
        auto newP = p;
        if (axis == 'x' && p.x > value) {
            newP.x = 2 * value - p.x;
        } else if (axis == 'y' && p.y > value) {
            newP.y = 2 * value - p.y;
        }
        newPoints[newP] = true;
    }

    writeln(newPoints.length);
}
