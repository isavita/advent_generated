
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <unordered_set>
#include <unordered_map>
#include <algorithm>

struct Coord {
    int x, y, z;

    bool operator==(const Coord& other) const {
        return x == other.x && y == other.y && z == other.z;
    }
};

namespace std {
    template <>
    struct hash<Coord> {
        size_t operator()(const Coord& c) const {
            size_t hx = hash<int>()(c.x);
            size_t hy = hash<int>()(c.y);
            size_t hz = hash<int>()(c.z);
            return hx ^ (hy << 1) ^ (hz << 2);
        }
    };
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::ifstream inputFile("input.txt");
    std::vector<std::string> lines;
    std::string line;
    while (std::getline(inputFile, line)) {
        lines.push_back(line);
    }
    inputFile.close();

    std::unordered_set<Coord> activeCubes;
    for (int y = 0; y < lines.size(); ++y) {
        for (int x = 0; x < lines[0].length(); ++x) {
            if (lines[y][x] == '#') {
                activeCubes.insert({x, y, 0});
            }
        }
    }

    for (int cycle = 0; cycle < 6; ++cycle) {
        std::unordered_set<Coord> newActiveCubes;
        std::unordered_map<Coord, int> inactiveNeighborsCount;

        for (const auto& cube : activeCubes) {
            int activeNeighborCount = 0;
            for (int dz = -1; dz <= 1; ++dz) {
                for (int dy = -1; dy <= 1; ++dy) {
                    for (int dx = -1; dx <= 1; ++dx) {
                        if (dx == 0 && dy == 0 && dz == 0) {
                            continue;
                        }
                        Coord neighbor = {cube.x + dx, cube.y + dy, cube.z + dz};
                        if (activeCubes.count(neighbor)) {
                            activeNeighborCount++;
                        } else {
                            inactiveNeighborsCount[neighbor]++;
                        }
                    }
                }
            }

            if (activeNeighborCount == 2 || activeNeighborCount == 3) {
                newActiveCubes.insert(cube);
            }
        }

        for (const auto& pair : inactiveNeighborsCount) {
            if (pair.second == 3) {
                newActiveCubes.insert(pair.first);
            }
        }
        activeCubes = std::move(newActiveCubes);
    }

    std::cout << activeCubes.size() << std::endl;

    return 0;
}
