
#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <algorithm>

int main() {
    std::ifstream inputFile("input.txt");
    std::vector<std::vector<int>> grid;
    std::string line;

    while (std::getline(inputFile, line)) {
        std::vector<int> row;
        for (char c : line) {
            row.push_back(c - '0');
        }
        grid.push_back(row);
    }
    inputFile.close();

    int R = grid.size();
    if (R == 0) {
        std::cout << 0 << std::endl;
        return 0;
    }
    int C = grid[0].size();
    if (C == 0) {
        std::cout << 0 << std::endl;
        return 0;
    }

    std::vector<std::vector<bool>> is_visible(R, std::vector<bool>(C, false));

    for (int r = 0; r < R; ++r) {
        int max_height = -1;
        for (int c = 0; c < C; ++c) {
            if (grid[r][c] > max_height) {
                is_visible[r][c] = true;
                max_height = grid[r][c];
            }
        }
    }

    for (int r = 0; r < R; ++r) {
        int max_height = -1;
        for (int c = C - 1; c >= 0; --c) {
            if (grid[r][c] > max_height) {
                is_visible[r][c] = true;
                max_height = grid[r][c];
            }
        }
    }

    for (int c = 0; c < C; ++c) {
        int max_height = -1;
        for (int r = 0; r < R; ++r) {
            if (grid[r][c] > max_height) {
                is_visible[r][c] = true;
                max_height = grid[r][c];
            }
        }
    }

    for (int c = 0; c < C; ++c) {
        int max_height = -1;
        for (int r = R - 1; r >= 0; --r) {
            if (grid[r][c] > max_height) {
                is_visible[r][c] = true;
                max_height = grid[r][c];
            }
        }
    }

    int visible_count = 0;
    for (int r = 0; r < R; ++r) {
        for (int c = 0; c < C; ++c) {
            if (is_visible[r][c]) {
                visible_count++;
            }
        }
    }

    std::cout << visible_count << std::endl;

    return 0;
}
