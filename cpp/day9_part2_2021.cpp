
#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <algorithm>

int R, C;
const int dx[] = {0, 0, 1, -1};
const int dy[] = {1, -1, 0, 0};

bool isLowPoint(const std::vector<std::vector<int>>& heightmap, int x, int y) {
    int height = heightmap[y][x];
    if (x > 0 && heightmap[y][x - 1] <= height) return false;
    if (x < C - 1 && heightmap[y][x + 1] <= height) return false;
    if (y > 0 && heightmap[y - 1][x] <= height) return false;
    if (y < R - 1 && heightmap[y + 1][x] <= height) return false;
    return true;
}

int exploreBasin(const std::vector<std::vector<int>>& heightmap, int x, int y, std::vector<std::vector<bool>>& visited) {
    if (x < 0 || x >= C || y < 0 || y >= R || visited[y][x] || heightmap[y][x] == 9) {
        return 0;
    }
    visited[y][x] = true;
    int size = 1;
    for (int i = 0; i < 4; ++i) {
        size += exploreBasin(heightmap, x + dx[i], y + dy[i], visited);
    }
    return size;
}

int main() {
    std::ifstream file("input.txt");
    std::vector<std::vector<int>> heightmap;
    std::string line;

    while (std::getline(file, line)) {
        std::vector<int> row;
        for (char c : line) {
            row.push_back(c - '0');
        }
        heightmap.push_back(row);
    }
    file.close();

    R = heightmap.size();
    if (R == 0) return 0;
    C = heightmap[0].size();
    if (C == 0) return 0;

    std::vector<int> basinSizes;
    std::vector<std::vector<bool>> visited(R, std::vector<bool>(C, false));

    for (int y = 0; y < R; ++y) {
        for (int x = 0; x < C; ++x) {
            if (isLowPoint(heightmap, x, y)) {
                int size = exploreBasin(heightmap, x, y, visited);
                basinSizes.push_back(size);
            }
        }
    }

    std::sort(basinSizes.rbegin(), basinSizes.rend());

    long long result = 0;
    if (basinSizes.size() >= 3) {
        result = static_cast<long long>(basinSizes[0]) * basinSizes[1] * basinSizes[2];
    } else {
        result = 1;
        for (int size : basinSizes) {
            result *= size;
        }
    }

    std::cout << result << std::endl;

    return 0;
}

