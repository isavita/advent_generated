
import std.stdio;
import std.file;
import std.string;
import std.conv;
import std.algorithm;
import std.range;
import std.numeric;

struct Point {
    int c, r;
}

void parseInput(string input, out char[][] matrix, out size_t originCol) {
    auto coordSets = input.strip.splitLines
        .map!(line => line.split(" -> ")
            .map!(p => p.split(','))
            .map!(parts => Point(parts[0].to!int, parts[1].to!int))
            .array)
        .array;

    int minCol = int.max;
    int maxRow = 0;
    foreach (coordSet; coordSets) {
        foreach (coord; coordSet) {
            minCol = min(minCol, coord.c);
            maxRow = max(maxRow, coord.r);
        }
    }

    const extraSpace = 200;
    int offset = minCol - extraSpace;
    int maxCol = 0;

    foreach (ref coordSet; coordSets) {
        foreach (ref coord; coordSet) {
            coord.c -= offset;
            maxCol = max(maxCol, coord.c);
        }
    }

    auto width = maxCol + extraSpace;
    auto height = maxRow + 3;

    matrix = new char[][](height, width);
    foreach (row; matrix) {
        row[] = '.';
    }

    foreach (coordSet; coordSets) {
        foreach (i; 1 .. coordSet.length) {
            auto p1 = coordSet[i - 1];
            auto p2 = coordSet[i];

            if (p1.c == p2.c) {
                foreach (r; min(p1.r, p2.r) .. max(p1.r, p2.r) + 1) {
                    matrix[r][p1.c] = '#';
                }
            } else {
                foreach (c; min(p1.c, p2.c) .. max(p1.c, p2.c) + 1) {
                    matrix[p1.r][c] = '#';
                }
            }
        }
    }
    originCol = 500 - offset;
}

void dropSand(ref char[][] matrix, size_t originCol) {
    size_t r = 0, c = originCol;
    auto height = matrix.length;

    while (r < height - 1) {
        if (matrix[r + 1][c] == '.') {
            r++;
        } else if (matrix[r + 1][c - 1] == '.') {
            r++;
            c--;
        } else if (matrix[r + 1][c + 1] == '.') {
            r++;
            c++;
        } else {
            matrix[r][c] = 'o';
            return;
        }
    }
}

long solve(string input) {
    char[][] matrix;
    size_t originCol;
    parseInput(input, matrix, originCol);

    foreach (ref cell; matrix[$-1]) {
        cell = '#';
    }

    long ans = 0;
    while (matrix[0][originCol] != 'o') {
        dropSand(matrix, originCol);
        ans++;
    }
    return ans;
}

void main() {
    solve(readText("input.txt")).writeln;
}
