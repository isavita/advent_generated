
#include <fstream>
#include <functional>
#include <iostream>
#include <string>
#include <unordered_map>
#include <unordered_set>

struct Point4D {
    int x, y, z, w;

    bool operator==(const Point4D& other) const {
        return x == other.x && y == other.y && z == other.z && w == other.w;
    }
};

namespace std {
    template <>
    struct hash<Point4D> {
        size_t operator()(const Point4D& p) const {
            size_t h1 = hash<int>()(p.x);
            size_t h2 = hash<int>()(p.y);
            size_t h3 = hash<int>()(p.z);
            size_t h4 = hash<int>()(p.w);
            return h1 ^ (h2 << 1) ^ (h3 << 2) ^ (h4 << 3);
        }
    };
} // namespace std

std::unordered_set<Point4D> simulateCycle4D(const std::unordered_set<Point4D>& activeCubes) {
    std::unordered_set<Point4D> newActiveCubes;
    std::unordered_map<Point4D, int> neighborCounts;

    for (const auto& coord : activeCubes) {
        for (int dw = -1; dw <= 1; ++dw) {
            for (int dz = -1; dz <= 1; ++dz) {
                for (int dy = -1; dy <= 1; ++dy) {
                    for (int dx = -1; dx <= 1; ++dx) {
                        if (dw == 0 && dz == 0 && dy == 0 && dx == 0) {
                            continue;
                        }
                        Point4D neighbor = {coord.x + dx, coord.y + dy, coord.z + dz, coord.w + dw};
                        neighborCounts[neighbor]++;
                    }
                }
            }
        }
    }

    for (const auto& pair : neighborCounts) {
        const Point4D& coord = pair.first;
        int count = pair.second;

        if (count == 3 || (count == 2 && activeCubes.count(coord))) {
            newActiveCubes.insert(coord);
        }
    }

    return newActiveCubes;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::ifstream file("input.txt");
    std::unordered_set<Point4D> activeCubes;
    std::string line;
    int y = 0;

    while (std::getline(file, line)) {
        for (int x = 0; x < line.length(); ++x) {
            if (line[x] == '#') {
                activeCubes.insert({x, y, 0, 0});
            }
        }
        y++;
    }
    file.close();

    for (int cycle = 0; cycle < 6; ++cycle) {
        activeCubes = simulateCycle4D(activeCubes);
    }

    std::cout << activeCubes.size() << std::endl;

    return 0;
}
