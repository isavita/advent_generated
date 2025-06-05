
#include <fstream>
#include <iostream>
#include <string>
#include <vector>
#include <set>
#include <utility> // For std::pair

int flash(std::vector<std::vector<int>>& grid, int x, int y, std::vector<std::vector<bool>>& flashed) {
    if (flashed[y][x]) {
        return 0;
    }

    flashed[y][x] = true;
    int currentFlashes = 1;

    int H = grid.size();
    int W = grid[0].size();

    int dr[] = {-1, -1, -1, 0, 0, 1, 1, 1};
    int dc[] = {-1, 0, 1, -1, 1, -1, 0, 1};

    for (int i = 0; i < 8; ++i) {
        int nr = y + dr[i];
        int nc = x + dc[i];

        if (nr >= 0 && nr < H && nc >= 0 && nc < W) {
            grid[nr][nc]++;
            if (grid[nr][nc] > 9) {
                currentFlashes += flash(grid, nc, nr, flashed);
            }
        }
    }
    return currentFlashes;
}

int simulateStep(std::vector<std::vector<int>>& grid) {
    int H = grid.size();
    int W = grid[0].size();
    std::vector<std::vector<bool>> flashedThisStep(H, std::vector<bool>(W, false));
    int totalFlashesInStep = 0;

    for (int r = 0; r < H; ++r) {
        for (int c = 0; c < W; ++c) {
            grid[r][c]++;
        }
    }

    for (int r = 0; r < H; ++r) {
        for (int c = 0; c < W; ++c) {
            if (grid[r][c] > 9 && !flashedThisStep[r][c]) {
                totalFlashesInStep += flash(grid, c, r, flashedThisStep);
            }
        }
    }

    for (int r = 0; r < H; ++r) {
        for (int c = 0; c < W; ++c) {
            if (flashedThisStep[r][c]) {
                grid[r][c] = 0;
            }
        }
    }
    return totalFlashesInStep;
}

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

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::vector<std::vector<int>> grid = readInput("input.txt");
    int totalFlashes = 0;

    for (int step = 0; step < 100; ++step) {
        totalFlashes += simulateStep(grid);
    }

    std::cout << totalFlashes << std::endl;

    return 0;
}
