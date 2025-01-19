
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>
#include <cmath>
#include <algorithm>

struct Point {
    int x, y;
};

struct Sensor {
    Point pos;
    Point beacon;
    int dist;
};

int manhattan(const Point& p, const Point& q) {
    return std::abs(p.x - q.x) + std::abs(p.y - q.y);
}

int main() {
    std::ifstream file("input.txt");
    std::string line;
    std::vector<Sensor> sensors;

    while (std::getline(file, line)) {
        Sensor s;
        std::sscanf(line.c_str(), "Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d", &s.pos.x, &s.pos.y, &s.beacon.x, &s.beacon.y);
        s.dist = manhattan(s.pos, s.beacon);
        sensors.push_back(s);
    }

    int maxcoord = 4000000;
    for (int x = 0; x <= maxcoord; ++x) {
        for (int y = 0; y <= maxcoord; ) {
            Point p = {x, y};
            bool detected = false;
            int skip = 0;
            for (const auto& s : sensors) {
                if (manhattan(s.pos, p) <= s.dist) {
                    detected = true;
                    int dist = s.dist - std::abs(s.pos.x - x);
                    skip = std::max(skip, dist + s.pos.y - y);
                }
            }
            if (!detected) {
                std::cout << (long long)x * 4000000 + y << std::endl;
                return 0;
            }
            y += std::max(1,skip);
        }
    }

    return 0;
}
