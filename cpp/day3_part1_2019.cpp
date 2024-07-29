#include <iostream>
#include <fstream>
#include <sstream>
#include <unordered_map>
#include <vector>
#include <cmath>
#include <limits>

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
            return hash<int>()(p.x) ^ hash<int>()(p.y);
        }
    };
}

std::vector<Point> getPoints(const std::string& path) {
    std::unordered_map<Point, bool> points;
    Point current = {0, 0};
    std::istringstream ss(path);
    std::string move;

    while (std::getline(ss, move, ',')) {
        char dir = move[0];
        int steps = std::stoi(move.substr(1));
        for (int i = 0; i < steps; ++i) {
            if (dir == 'U') current.y++;
            else if (dir == 'D') current.y--;
            else if (dir == 'L') current.x--;
            else if (dir == 'R') current.x++;
            points[current] = true;
        }
    }
    std::vector<Point> result;
    for (const auto& p : points) {
        result.push_back(p.first);
    }
    return result;
}

int main() {
    std::ifstream file("input.txt");
    std::string line1, line2;
    std::getline(file, line1);
    std::getline(file, line2);

    auto wire1 = getPoints(line1);
    auto wire2 = getPoints(line2);

    std::unordered_map<Point, bool> intersections;
    for (const auto& p : wire1) {
        intersections[p] = true;
    }

    int minDistance = std::numeric_limits<int>::max();
    for (const auto& p : wire2) {
        if (intersections[p]) {
            minDistance = std::min(minDistance, std::abs(p.x) + std::abs(p.y));
        }
    }

    std::cout << minDistance << std::endl;
    return 0;
}