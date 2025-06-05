
#include <iostream>
#include <fstream>
#include <vector>
#include <set>
#include <map>
#include <algorithm>

struct Point {
    int x, y;

    bool operator<(const Point& other) const {
        if (y != other.y) return y < other.y;
        return x < other.x;
    }

    Point operator+(const Point& other) const {
        return {x + other.x, y + other.y};
    }
};

struct DirectionInfo {
    Point move;
    std::vector<Point> checks;
};

const std::vector<Point> ADJACENT_OFFSETS = {
    {-1, -1}, {0, -1}, {1, -1},
    {-1, 0},           {1, 0},
    {-1, 1}, {0, 1}, {1, 1}
};

const std::vector<DirectionInfo> ALL_DIRECTIONS = {
    {{0, -1}, {{0, -1}, {1, -1}, {-1, -1}}},
    {{0, 1},  {{0, 1}, {1, 1}, {-1, 1}}},
    {{-1, 0}, {{-1, 0}, {-1, -1}, {-1, 1}}},
    {{1, 0},  {{1, 0}, {1, -1}, {1, 1}}}
};

std::set<Point> readInput(const std::string& filePath) {
    std::set<Point> elves;
    std::ifstream file(filePath);
    std::string line;
    int y = 0;
    while (std::getline(file, line)) {
        for (int x = 0; x < line.length(); ++x) {
            if (line[x] == '#') {
                elves.insert({x, y});
            }
        }
        y++;
    }
    return elves;
}

std::set<Point> simulateRounds(std::set<Point> elves, int rounds) {
    int currentDirIdx = 0;

    for (int roundNum = 0; roundNum < rounds; ++roundNum) {
        std::map<Point, std::vector<Point>> proposals;

        for (const auto& elf : elves) {
            bool hasNeighbor = false;
            for (const auto& offset : ADJACENT_OFFSETS) {
                if (elves.count(elf + offset)) {
                    hasNeighbor = true;
                    break;
                }
            }

            if (!hasNeighbor) {
                continue;
            }

            for (int i = 0; i < 4; ++i) {
                const auto& dir = ALL_DIRECTIONS[(currentDirIdx + i) % 4];
                bool canMove = true;
                for (const auto& checkOffset : dir.checks) {
                    if (elves.count(elf + checkOffset)) {
                        canMove = false;
                        break;
                    }
                }

                if (canMove) {
                    proposals[elf + dir.move].push_back(elf);
                    break;
                }
            }
        }

        std::set<Point> newElves = elves;
        for (const auto& pair : proposals) {
            const Point& dest = pair.first;
            const std::vector<Point>& proposers = pair.second;
            if (proposers.size() == 1) {
                newElves.erase(proposers[0]);
                newElves.insert(dest);
            }
        }
        elves = newElves;

        currentDirIdx = (currentDirIdx + 1) % 4;
    }
    return elves;
}

long long calculateEmptyGround(const std::set<Point>& elves) {
    if (elves.empty()) {
        return 0;
    }

    int minX = elves.begin()->x;
    int maxX = elves.begin()->x;
    int minY = elves.begin()->y;
    int maxY = elves.begin()->y;

    for (const auto& elf : elves) {
        minX = std::min(minX, elf.x);
        maxX = std::max(maxX, elf.x);
        minY = std::min(minY, elf.y);
        maxY = std::max(maxY, elf.y);
    }

    long long width = maxX - minX + 1;
    long long height = maxY - minY + 1;
    long long totalTiles = width * height;
    long long emptyTiles = totalTiles - elves.size();
    return emptyTiles;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::set<Point> elves = readInput("input.txt");
    elves = simulateRounds(elves, 10);
    long long emptyGround = calculateEmptyGround(elves);
    std::cout << emptyGround << std::endl;

    return 0;
}
