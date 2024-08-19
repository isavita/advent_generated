import std.stdio;
import std.file;
import std.conv;
import std.algorithm;
import std.array;

const favoriteNumber = 1362; // Replace with your puzzle input

struct Point {
    int x, y;
}

bool isWall(int x, int y) {
    int num = x * x + 3 * x + 2 * x * y + y + y * y + favoriteNumber;
    int bits = 0;
    while (num > 0) {
        if (num % 2 == 1) bits++;
        num /= 2;
    }
    return bits % 2 != 0;
}

int bfs(Point start, Point target) {
    bool[Point] visited;
    Point[] queue;
    int[] steps;
    queue ~= start;
    steps ~= 0;
    visited[start] = true;

    while (queue.length > 0) {
        Point point = queue[0];
        int step = steps[0];
        queue = queue[1..$];
        steps = steps[1..$];

        if (point == target) {
            return step;
        }

        foreach (Point delta; [Point(1, 0), Point(-1, 0), Point(0, 1), Point(0, -1)]) {
            Point next = Point(point.x + delta.x, point.y + delta.y);
            if (next.x >= 0 && next.y >= 0 && !isWall(next.x, next.y) && !(next in visited)) {
                visited[next] = true;
                queue ~= next;
                steps ~= step + 1;
            }
        }
    }
    return -1; // If target is unreachable
}

void main() {
    Point start = Point(1, 1);
    Point target = Point(31, 39);
    int steps = bfs(start, target);
    writeln(steps);
}