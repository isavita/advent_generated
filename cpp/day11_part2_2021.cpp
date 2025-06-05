
#include <fstream>
#include <iostream>
#include <set>
#include <string>
#include <utility>
#include <vector>

const int DIRS[8][2] = {
    {-1, -1}, {-1, 0}, {-1, 1}, {0, -1}, {0, 1}, {1, -1}, {1, 0}, {1, 1}};

std::vector<std::vector<int>> readInput(const std::string& filename) {
    std::vector<std::vector<int>> grid;
    std::ifstream file(filename);
    std::string line;
    while (std::getline(file, line)) {
        std::vector<int> row;
        for (char c : line) {
            row.push_back(c - '0');
        }
        grid.push_back(row);
    }
    return grid;
}

int flash(std::vector<std::vector<int>>& grid, int x, int y,
          std::set<std::pair<int, int>>& flashed) {
    if (flashed.count({x, y})) {
        return 0;
    }

    flashed.insert({x, y});
    int flashesCount = 1;
    int R = grid.size();
    int C = grid[0].size();

    for (int i = 0; i < 8; ++i) {
        int newX = x + DIRS[i][0];
        int newY = y + DIRS[i][1];

        if (newX >= 0 && newX < C && newY >= 0 && newY < R) {
            grid[newY][newX]++;
            if (grid[newY][newX] > 9) {
                flashesCount += flash(grid, newX, newY, flashed);
            }
        }
    }
    return flashesCount;
}

int simulateStep(std::vector<std::vector<int>>& grid) {
    int totalFlashes = 0;
    std::set<std::pair<int, int>> flashedThisStep;
    int R = grid.size();
    int C = grid[0].size();

    for (int y = 0; y < R; ++y) {
        for (int x = 0; x < C; ++x) {
            grid[y][x]++;
        }
    }

    for (int y = 0; y < R; ++y) {
        for (int x = 0; x < C; ++x) {
            if (grid[y][x] > 9) {
                totalFlashes += flash(grid, x, y, flashedThisStep);
            }
        }
    }

    for (const auto& p : flashedThisStep) {
        grid[p.second][p.first] = 0;
    }

    return totalFlashes;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::vector<std::vector<int>> grid = readInput("input.txt");

    int step = 0;
    int grid_size = grid.size() * grid[0].size();
    while (true) {
        step++;
        int flashes = simulateStep(grid);
        if (flashes == grid_size) {
            break;
        }
    }

    std::cout << step << std::endl;

    return 0;
}
