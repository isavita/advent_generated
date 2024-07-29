#include <iostream>
#include <fstream>
#include <sstream>
#include <unordered_map>

struct Point {
    int x, y, z;
    
    bool operator==(const Point& other) const {
        return x == other.x && y == other.y && z == other.z;
    }
};

namespace std {
    template <>
    struct hash<Point> {
        size_t operator()(const Point& p) const {
            return hash<int>()(p.x) ^ hash<int>()(p.y) ^ hash<int>()(p.z);
        }
    };
}

int calculateExposedSides(const Point& p, const std::unordered_map<Point, bool>& cubes) {
    Point directions[] = {{1, 0, 0}, {-1, 0, 0}, {0, 1, 0}, {0, -1, 0}, {0, 0, 1}, {0, 0, -1}};
    int exposedSides = 6;
    for (const auto& dir : directions) {
        Point adjacent = {p.x + dir.x, p.y + dir.y, p.z + dir.z};
        if (cubes.count(adjacent)) exposedSides--;
    }
    return exposedSides;
}

int main() {
    std::ifstream file("input.txt");
    std::unordered_map<Point, bool> cubes;
    std::string line;

    while (std::getline(file, line)) {
        std::istringstream iss(line);
        std::string token;
        Point p;
        std::getline(iss, token, ','); p.x = std::stoi(token);
        std::getline(iss, token, ','); p.y = std::stoi(token);
        std::getline(iss, token); p.z = std::stoi(token);
        cubes[p] = true;
    }

    int surfaceArea = 0;
    for (const auto& cube : cubes) {
        surfaceArea += calculateExposedSides(cube.first, cubes);
    }

    std::cout << surfaceArea << std::endl;
    return 0;
}