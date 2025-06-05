
#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <queue>
#include <utility>

std::pair<long long, long long> calculate_region(const std::vector<std::string>& grid, int r_start, int c_start, std::vector<std::vector<bool>>& visited) {
    int rows = grid.size();
    int cols = grid[0].length();
    char region_char = grid[r_start][c_start];
    long long area = 0;
    long long perimeter = 0;

    std::queue<std::pair<int, int>> q;
    q.push({r_start, c_start});
    visited[r_start][c_start] = true;

    int dr[] = {-1, 1, 0, 0};
    int dc[] = {0, 0, -1, 1};

    while (!q.empty()) {
        std::pair<int, int> current = q.front();
        q.pop();
        int r = current.first;
        int c = current.second;

        area++;

        for (int i = 0; i < 4; ++i) {
            int nr = r + dr[i];
            int nc = c + dc[i];

            if (nr < 0 || nr >= rows || nc < 0 || nc >= cols) {
                perimeter++;
            } else if (grid[nr][nc] != region_char) {
                perimeter++;
            } else if (!visited[nr][nc]) {
                visited[nr][nc] = true;
                q.push({nr, nc});
            }
        }
    }
    return {area, perimeter};
}

long long solve() {
    std::ifstream infile("input.txt");
    std::vector<std::string> grid;
    std::string line;

    while (std::getline(infile, line)) {
        if (!line.empty()) {
            grid.push_back(line);
        }
    }
    infile.close();

    if (grid.empty() || grid[0].empty()) {
        return 0;
    }

    int rows = grid.size();
    int cols = grid[0].length();
    std::vector<std::vector<bool>> visited(rows, std::vector<bool>(cols, false));
    long long total_price = 0;

    for (int r = 0; r < rows; ++r) {
        for (int c = 0; c < cols; ++c) {
            if (!visited[r][c]) {
                std::pair<long long, long long> result = calculate_region(grid, r, c, visited);
                total_price += result.first * result.second;
            }
        }
    }
    return total_price;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::cout << solve() << std::endl;

    return 0;
}
