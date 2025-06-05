
#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <utility>
#include <algorithm>

int main() {
    std::ifstream file("input.txt");
    std::vector<std::vector<int>> grid;
    std::string line;

    while (std::getline(file, line)) {
        if (line.empty()) continue;
        std::vector<int> row;
        for (char c : line) {
            row.push_back(c - '0');
        }
        grid.push_back(row);
    }
    file.close();

    if (grid.empty()) {
        std::cout << 0 << std::endl;
        return 0;
    }

    int rows = grid.size();
    int cols = grid[0].size();

    std::pair<int, int> neighbors4[] = {{0, 1}, {0, -1}, {1, 0}, {-1, 0}};

    long long max_score = 0;

    for (int r = 0; r < rows; ++r) {
        for (int c = 0; c < cols; ++c) {
            int current_height = grid[r][c];
            long long score = 1;

            for (const auto& dir : neighbors4) {
                int dr = dir.first;
                int dc = dir.second;

                int view = 0;
                int nr = r;
                int nc = c;

                while (true) {
                    nr += dr;
                    nc += dc;

                    if (nr >= 0 && nr < rows && nc >= 0 && nc < cols) {
                        view++;
                        if (grid[nr][nc] >= current_height) {
                            score *= view;
                            break;
                        }
                    } else {
                        score *= view;
                        break;
                    }
                }
            }
            if (score > max_score) {
                max_score = score;
            }
        }
    }

    std::cout << max_score << std::endl;

    return 0;
}
