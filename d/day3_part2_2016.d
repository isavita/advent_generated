import std.stdio;
import std.file;
import std.conv;
import std.array; // Added import for std.array to use split

bool isValidTriangle(int a, int b, int c) {
    return a + b > c && a + c > b && b + c > a;
}

void main() {
    int[][] triangles;
    foreach (line; File("input.txt").byLine()) {
        auto sides = line.split;
        if (sides.length != 3) {
            writeln("Invalid input format");
            return;
        }
        int a = to!int(sides[0]);
        int b = to!int(sides[1]);
        int c = to!int(sides[2]);
        triangles ~= [a, b, c];
    }

    int count = 0;
    foreach (triangle; triangles) {
        if (isValidTriangle(triangle[0], triangle[1], triangle[2])) {
            count++;
        }
    }

    int countByColumns = 0;
    foreach (i; 0 .. triangles.length) { // Fixed loop syntax
        if (i % 3 == 0) {
            foreach (j; 0 .. 3) {
                if (isValidTriangle(triangles[i][j], triangles[i+1][j], triangles[i+2][j])) {
                    countByColumns++;
                }
            }
        }
    }

    writeln(count);
    writeln(countByColumns);
}