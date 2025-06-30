
import std.stdio;
import std.file;
import std.string;
import std.conv;
import std.algorithm;
import std.array;

struct Point {
    int x;
    int y;
}

void main() {
    bool[Point] points;
    Point[] folds;
    bool readingPoints = true;

    foreach (line; readText("input.txt").split("\n")) {
        auto stripped = line.strip();
        if (stripped.empty) {
            readingPoints = false;
            continue;
        }

        if (readingPoints) {
            auto parts = stripped.split(',');
            points[Point(parts[0].to!int, parts[1].to!int)] = true;
        } else {
            auto parts = stripped.split('=');
            auto val = parts[1].to!int;
            if (parts[0].endsWith("x")) {
                folds ~= Point(val, 0);
            } else {
                folds ~= Point(0, val);
            }
        }
    }

    foreach (i, fold; folds) {
        bool[Point] newPoints;
        foreach (p; points.keys) {
            auto newP = p;
            if (fold.x != 0 && newP.x > fold.x) {
                newP.x = 2 * fold.x - newP.x;
            } else if (fold.y != 0 && newP.y > fold.y) {
                newP.y = 2 * fold.y - newP.y;
            }
            newPoints[newP] = true;
        }
        points = newPoints;

        if (i == 0) {
            writeln(points.length);
        }
    }

    int maxX = 0;
    int maxY = 0;
    foreach (p; points.keys) {
        maxX = max(maxX, p.x);
        maxY = max(maxY, p.y);
    }

    auto grid = new char[][](maxY + 1, maxX + 1);
    foreach (ref row; grid) {
        row[] = ' ';
    }

    foreach (p; points.keys) {
        grid[p.y][p.x] = '#';
    }

    foreach (row; grid) {
        writeln(row);
    }
}
