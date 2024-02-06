
#include <iostream>
#include <fstream>
#include <vector>
#include <map>
#include <cmath>

struct Point {
    int x;
    int y;
};

struct Sensor {
    Point pos;
    Point beacon;
    int dist;
};

int abs(int n) {
    if (n < 0) {
        return -n;
    }
    return n;
}

int manhattan(Point p, Point q) {
    return abs(p.x - q.x) + abs(p.y - q.y);
}

int impossible(std::vector<Sensor> sensors, int y) {
    std::map<int, bool> pts;
    for (const auto& s : sensors) {
        int dist = s.dist - abs(s.pos.y - y);
        for (int x = 0; x <= dist; x++) {
            pts[s.pos.x + x] = true;
            pts[s.pos.x - x] = true;
        }
    }
    for (const auto& s : sensors) {
        if (s.beacon.y == y) {
            pts.erase(s.beacon.x);
        }
    }
    return pts.size();
}

int main() {
    std::vector<Sensor> sensors;
    std::ifstream file("input.txt");
    if (file.is_open()) {
        std::string line;
        while (std::getline(file, line)) {
            Sensor s;
            sscanf(line.c_str(), "Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d", &s.pos.x, &s.pos.y, &s.beacon.x, &s.beacon.y);
            s.dist = manhattan(s.pos, s.beacon);
            sensors.push_back(s);
        }
        file.close();
    }
    std::cout << impossible(sensors, 2000000) << std::endl;
    return 0;
}
