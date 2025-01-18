
#include <iostream>
#include <fstream>
#include <sstream>
#include <unordered_set>
#include <vector>

struct Point {
    int x, y;
    bool operator==(const Point& other) const {
        return x == other.x && y == other.y;
    }
};

struct PointHash {
    std::size_t operator()(const Point& p) const {
        return std::hash<int>()(p.x) ^ std::hash<int>()(p.y);
    }
};

Point move(Point p, char dir) {
    switch (dir) {
        case 'U': return {p.x, p.y + 1};
        case 'R': return {p.x + 1, p.y};
        case 'D': return {p.x, p.y - 1};
        case 'L': return {p.x - 1, p.y};
    }
    return p;
}

Point follow(Point head, Point tail) {
    int dx = head.x - tail.x;
    int dy = head.y - tail.y;
    if (std::abs(dx) <= 1 && std::abs(dy) <= 1) return tail;
    return {tail.x + (dx ? dx / std::abs(dx) : 0), tail.y + (dy ? dy / std::abs(dy) : 0)};
}

int main() {
    std::ifstream file("input.txt");
    std::string line;
    std::vector<Point> rope(10);
    std::unordered_set<Point, PointHash> visited;
    visited.insert(rope.back());

    while (std::getline(file, line)) {
        char dir;
        int steps;
        std::istringstream iss(line);
        iss >> dir >> steps;

        for (int i = 0; i < steps; ++i) {
            rope[0] = move(rope[0], dir);
            for (int j = 1; j < 10; ++j) {
                rope[j] = follow(rope[j - 1], rope[j]);
            }
            visited.insert(rope.back());
        }
    }

    std::cout << visited.size() << std::endl;
    return 0;
}
