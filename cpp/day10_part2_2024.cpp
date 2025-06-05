
#include <iostream>
#include <vector>
#include <string>
#include <fstream>

std::vector<std::vector<int>> grid;
std::vector<std::vector<long long>> dp;
int nr, nc;

const int dr[] = {1, -1, 0, 0};
const int dc[] = {0, 0, 1, -1};
const int NUM_DIRS = 4;

long long dfs(int r, int c) {
    if (dp[r][c] != -1) {
        return dp[r][c];
    }

    int h = grid[r][c];
    if (h == 9) {
        dp[r][c] = 1;
        return 1;
    }

    long long sum_paths = 0;
    for (int i = 0; i < NUM_DIRS; ++i) {
        int nr2 = r + dr[i];
        int nc2 = c + dc[i];

        if (nr2 >= 0 && nr2 < nr && nc2 >= 0 && nc2 < nc && grid[nr2][nc2] == h + 1) {
            sum_paths += dfs(nr2, nc2);
        }
    }

    dp[r][c] = sum_paths;
    return sum_paths;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::ifstream inputFile("input.txt");
    if (!inputFile.is_open()) {
        return 1;
    }

    std::string line;
    std::vector<std::string> lines_str;
    while (std::getline(inputFile, line)) {
        lines_str.push_back(line);
    }
    inputFile.close();

    nr = lines_str.size();
    if (nr == 0) {
        std::cout << 0 << std::endl;
        return 0;
    }
    nc = lines_str[0].length();
    if (nc == 0) {
        std::cout << 0 << std::endl;
        return 0;
    }

    grid.resize(nr, std::vector<int>(nc));
    dp.resize(nr, std::vector<long long>(nc, -1));

    for (int r_idx = 0; r_idx < nr; ++r_idx) {
        for (int c_idx = 0; c_idx < nc; ++c_idx) {
            grid[r_idx][c_idx] = lines_str[r_idx][c_idx] - '0';
        }
    }

    long long total = 0;
    for (int r_idx = 0; r_idx < nr; ++r_idx) {
        for (int c_idx = 0; c_idx < nc; ++c_idx) {
            if (grid[r_idx][c_idx] == 0) {
                total += dfs(r_idx, c_idx);
            }
        }
    }

    std::cout << total << std::endl;

    return 0;
}
