
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <queue>
#include <algorithm>
#include <stack>
#include <functional>

struct Point {
    int x, y;

    Point() : x(0), y(0) {}
    Point(int _x, int _y) : x(_x), y(_y) {}

    bool operator==(const Point& other) const {
        return x == other.x && y == other.y;
    }
};

struct PointHash {
    size_t operator()(const Point& p) const {
        size_t h1 = std::hash<int>{}(p.x);
        size_t h2 = std::hash<int>{}(p.y);
        return h1 ^ (h2 + 0x9e3779b9 + (h1 << 6) + (h1 >> 2));
    }
};

using DoorMap = std::unordered_map<Point, std::unordered_set<Point, PointHash>, PointHash>;

Point move(Point p, char dir) {
    if (dir == 'N') {
        return Point(p.x, p.y - 1);
    }
    if (dir == 'S') {
        return Point(p.x, p.y + 1);
    }
    if (dir == 'E') {
        return Point(p.x + 1, p.y);
    }
    if (dir == 'W') {
        return Point(p.x - 1, p.y);
    }
    return p;
}

DoorMap build_map(const std::string& regex) {
    DoorMap dm;
    std::stack<Point> s;
    Point cp(0, 0);

    for (char c : regex) {
        if (c == '(') {
            s.push(cp);
        } else if (c == '|') {
            cp = s.top();
        } else if (c == ')') {
            cp = s.top();
            s.pop();
        } else {
            Point np = move(cp, c);
            dm[cp].insert(np);
            cp = np;
        }
    }
    return dm;
}

int find_furthest_room(const DoorMap& dm) {
    std::unordered_map<Point, int, PointHash> visited_distances;
    std::queue<Point> q;

    Point start_point(0, 0);
    visited_distances[start_point] = 0;
    q.push(start_point);

    int max_doors = 0;

    while (!q.empty()) {
        Point p = q.front();
        q.pop();

        auto it = dm.find(p);
        if (it != dm.end()) {
            for (const Point& np : it->second) {
                if (visited_distances.find(np) == visited_distances.end()) {
                    int current_dist = visited_distances[p] + 1;
                    visited_distances[np] = current_dist;
                    max_doors = std::max(max_doors, current_dist);
                    q.push(np);
                }
            }
        }
    }
    return max_doors;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::ifstream file("input.txt");
    std::string regex;
    if (!file.is_open()) {
        return 1;
    }
    std::getline(file, regex);
    file.close();

    std::string core_regex = regex.substr(1, regex.length() - 2);

    DoorMap dm = build_map(core_regex);
    int max_doors = find_furthest_room(dm);
    std::cout << max_doors << std::endl;

    return 0;
}
