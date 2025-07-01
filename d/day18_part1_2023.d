
import std.stdio, std.file, std.string, std.conv, std.math, std.algorithm;

struct Coord {
    long x, y;
}

void main() {
    auto lines = readText("input.txt").strip.splitLines;

    Coord[] vertices;
    auto current = Coord(0, 0);
    vertices ~= current;
    long perimeter = 0;

    foreach (line; lines) {
        auto parts = line.split;
        auto dir = parts[0][0];
        auto len = parts[1].to!long;

        perimeter += len;

        final switch (dir) {
            case 'U': current.y -= len; break;
            case 'L': current.x -= len; break;
            case 'D': current.y += len; break;
            case 'R': current.x += len; break;
        }
        vertices ~= current;
    }

    long area = 0;
    foreach (i; 0 .. vertices.length) {
        auto j = (i + 1) % vertices.length;
        area += vertices[i].x * vertices[j].y;
        area -= vertices[i].y * vertices[j].x;
    }

    auto totalArea = abs(area) / 2 + perimeter / 2 + 1;
    writeln(totalArea);
}
