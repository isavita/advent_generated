import std.stdio;
import std.file;
import std.string;
import std.conv;

bool isValidTriangle(int a, int b, int c) {
    return a + b > c && a + c > b && b + c > a;
}

void main() {
    auto file = File("input.txt", "r");
    int validTriangles = 0;

    foreach (line; file.byLine()) {
        auto sides = split(strip(line));
        if (sides.length != 3) {
            writeln("Invalid input format");
            continue;
        }

        int a = to!int(sides[0]);
        int b = to!int(sides[1]);
        int c = to!int(sides[2]);

        if (isValidTriangle(a, b, c)) {
            validTriangles++;
        }
    }

    writeln(validTriangles);
}