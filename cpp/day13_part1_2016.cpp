
#include <iostream>
#include <fstream>
#include <queue>
#include <set>

using namespace std;

struct Point {
    int x, y;
    bool operator<(const Point& other) const {
        if (x != other.x) return x < other.x;
        return y < other.y;
    }
};

bool isWall(int favoriteNumber, int x, int y) {
    int num = x * x + 3 * x + 2 * x * y + y + y * y + favoriteNumber;
    int bits = 0;
    while (num > 0) {
        bits += num & 1;
        num >>= 1;
    }
    return bits & 1;
}

int bfs(Point start, Point target, int favoriteNumber) {
    set<Point> visited;
    queue<Point> q;
    q.push(start);
    visited.insert(start);
    int steps = 0;

    while (!q.empty()) {
        int size = q.size();
        for (int i = 0; i < size; ++i) {
            Point point = q.front();
            q.pop();
            if (point.x == target.x && point.y == target.y) {
                return steps;
            }

            int dx[] = {1, -1, 0, 0};
            int dy[] = {0, 0, 1, -1};
            for (int j = 0; j < 4; ++j) {
                Point next = {point.x + dx[j], point.y + dy[j]};
                if (next.x >= 0 && next.y >= 0 && !isWall(favoriteNumber, next.x, next.y) && visited.find(next) == visited.end()) {
                    visited.insert(next);
                    q.push(next);
                }
            }
        }
        steps++;
    }

    return -1;
}

int main() {
    ifstream inputFile("input.txt");
    int favoriteNumber;
    inputFile >> favoriteNumber;
    inputFile.close();

    Point start = {1, 1};
    Point target = {31, 39};
    int steps = bfs(start, target, favoriteNumber);
    cout << steps << endl;

    return 0;
}
