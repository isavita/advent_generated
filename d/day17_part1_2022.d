
import std.stdio;
import std.file;
import std.string;
import std.algorithm;

struct Point {
    int x, y;
}

bool hasCollision(const bool[Point] grid, const Point[] rock, Point pos) {
    foreach (const p; rock) {
        Point rockPos = Point(p.x + pos.x, p.y + pos.y);
        if (rockPos.x < 0 || rockPos.x > 6 || (rockPos in grid)) {
            return true;
        }
    }
    return false;
}

void main() {
    immutable jetPattern = readText("input.txt").strip();

    immutable Point[][] rocks = [
        [Point(0, 0), Point(1, 0), Point(2, 0), Point(3, 0)],
        [Point(1, 2), Point(0, 1), Point(1, 1), Point(2, 1), Point(1, 0)],
        [Point(2, 2), Point(2, 1), Point(0, 0), Point(1, 0), Point(2, 0)],
        [Point(0, 3), Point(0, 2), Point(0, 1), Point(0, 0)],
        [Point(0, 1), Point(1, 1), Point(0, 0), Point(1, 0)]
    ];

    bool[Point] grid;
    for (int x = 0; x < 7; ++x) {
        grid[Point(x, 0)] = true;
    }

    long floor = 0;
    size_t jetIndex = 0;

    for (long i = 0; i < 2022; ++i) {
        const currRock = rocks[i % 5];
        Point pos = Point(2, cast(int)floor + 4);

        while (true) {
            char jet = jetPattern[jetIndex];
            jetIndex = (jetIndex + 1) % jetPattern.length;

            Point nextPos = pos;
            nextPos.x += (jet == '>') ? 1 : -1;

            if (!hasCollision(grid, currRock, nextPos)) {
                pos = nextPos;
            }

            Point downPos = pos;
            downPos.y--;

            if (hasCollision(grid, currRock, downPos)) {
                foreach (const p; currRock) {
                    Point rockPos = Point(p.x + pos.x, p.y + pos.y);
                    grid[rockPos] = true;
                    floor = max(floor, rockPos.y);
                }
                break;
            }
            pos = downPos;
        }
    }

    writeln(floor);
}
