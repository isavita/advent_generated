import std.stdio;
import std.file;
import std.conv;

const favoriteNumber = 1362;

struct Point {
    int x, y;
}

bool isWall(int x, int y) {
    int num = x*x + 3*x + 2*x*y + y + y*y + favoriteNumber;
    int bits;
    while (num > 0) {
        if (num % 2 == 1) bits++;
        num /= 2;
    }
    return bits % 2 != 0;
}

int bfsMaxSteps(Point start, int maxSteps) {
    bool[Point] visited;
    Point[] queue;
    queue ~= start;
    visited[start] = true;
    ulong steps;  // Change type to ulong
    while (queue.length > 0 && steps < cast(ulong)maxSteps) {
        ulong size = cast(ulong)queue.length;  // Cast to ulong
        for (ulong i = 0; i < size; i++) {
            Point point = queue[i];
            foreach (Point delta; [Point(1, 0), Point(-1, 0), Point(0, 1), Point(0, -1)]) {
                Point next = Point(point.x + delta.x, point.y + delta.y);
                if (next.x >= 0 && next.y >= 0 && !isWall(next.x, next.y) && !(next in visited)) {
                    visited[next] = true;
                    queue ~= next;
                }
            }
        }
        queue = queue[size..$];
        steps++;
    }
    return cast(int)visited.length;  // Cast to int
}

void main() {
    Point start = Point(1, 1);
    int reachableLocations = bfsMaxSteps(start, 50);
    writeln(reachableLocations);
}