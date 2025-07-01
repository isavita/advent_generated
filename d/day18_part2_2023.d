
import std.stdio;
import std.file;
import std.string;
import std.conv;
import std.math;
import std.algorithm;

struct Coord {
    long x, y;
}

void main() {
    auto lines = readText("input.txt").strip().split("\n");

    auto current = Coord(0, 0);
    Coord[] vertices;
    vertices.reserve(lines.length + 1);
    vertices ~= current;

    long perimeter = 0;

    foreach (line; lines) {
        auto hexCode = line.split[2];
        auto length = to!long(hexCode[2 .. 7], 16);
        auto dirChar = hexCode[7];

        perimeter += length;

        Coord dir;
        switch (dirChar) {
            case '0': dir = Coord(1, 0); break;
            case '1': dir = Coord(0, 1); break;
            case '2': dir = Coord(-1, 0); break;
            case '3': dir = Coord(0, -1); break;
            default: assert(0);
        }

        current.x += dir.x * length;
        current.y += dir.y * length;
        vertices ~= current;
    }

    long shoelaceSum = 0;
    for (size_t i = 0; i < vertices.length - 1; ++i) {
        shoelaceSum += vertices[i].x * vertices[i+1].y;
        shoelaceSum -= vertices[i+1].x * vertices[i].y;
    }

    long totalArea = (abs(shoelaceSum) + perimeter) / 2 + 1;

    writeln(totalArea);
}
