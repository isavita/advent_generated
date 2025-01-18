
#include <iostream>
#include <fstream>
#include <queue>
#include <unordered_set>

using namespace std;

const int favoriteNumber = 1362;

struct Point {
    int x, y;
    bool operator==(const Point& other) const {
        return x == other.x && y == other.y;
    }
};

namespace std {
    template <>
    struct hash<Point> {
        size_t operator()(const Point& p) const {
            return ((hash<int>()(p.x) ^ (hash<int>()(p.y) << 1)) >> 1);
        }
    };
}

bool isWall(int x, int y) {
    int num = x * x + 3 * x + 2 * x * y + y + y * y + favoriteNumber;
    int bits = 0;
    while (num > 0) {
        bits += (num & 1);
        num >>= 1;
    }
    return bits % 2 != 0;
}

int bfsMaxSteps(Point start, int maxSteps) {
    unordered_set<Point> visited;
    queue<Point> q;
    q.push(start);
    visited.insert(start);
    int steps = 0;

    while (!q.empty() && steps < maxSteps) {
        int size = q.size();
        for (int i = 0; i < size; ++i) {
            Point point = q.front();
            q.pop();

            int dx[] = {1, -1, 0, 0};
            int dy[] = {0, 0, 1, -1};

            for (int j = 0; j < 4; ++j) {
                Point next = {point.x + dx[j], point.y + dy[j]};
                if (next.x >= 0 && next.y >= 0 && !isWall(next.x, next.y) && visited.find(next) == visited.end()) {
                    visited.insert(next);
                    q.push(next);
                }
            }
        }
        steps++;
    }
    return visited.size();
}

int main() {
    Point start = {1, 1};
    int reachableLocations = bfsMaxSteps(start, 50);
    cout << reachableLocations << endl;
    return 0;
}
