
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <queue>
#include <tuple>
#include <utility> // For std::pair

const int GRID_SIZE = 71;

int bfs(const std::vector<std::vector<char>>& grid) {
    std::queue<std::tuple<int, int, int>> q;
    std::vector<std::vector<bool>> visited(GRID_SIZE, std::vector<bool>(GRID_SIZE, false));

    q.push({0, 0, 0});
    visited[0][0] = true;

    int dr[] = {0, 0, 1, -1};
    int dc[] = {1, -1, 0, 0};

    while (!q.empty()) {
        auto [r, c, dist] = q.front();
        q.pop();

        if (r == GRID_SIZE - 1 && c == GRID_SIZE - 1) {
            return dist;
        }

        for (int i = 0; i < 4; ++i) {
            int nr = r + dr[i];
            int nc = c + dc[i];

            if (nr >= 0 && nr < GRID_SIZE && nc >= 0 && nc < GRID_SIZE &&
                grid[nr][nc] == '.' && !visited[nr][nc]) {
                visited[nr][nc] = true;
                q.push({nr, nc, dist + 1});
            }
        }
    }
    return -1;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::vector<std::pair<int, int>> byte_positions;
    std::ifstream file("input.txt");
    std::string line;

    while (std::getline(file, line)) {
        size_t comma_pos = line.find(',');
        int x = std::stoi(line.substr(0, comma_pos));
        int y = std::stoi(line.substr(comma_pos + 1));
        byte_positions.push_back({x, y});
    }
    file.close();

    // Part 1
    std::vector<std::vector<char>> grid1(GRID_SIZE, std::vector<char>(GRID_SIZE, '.'));
    for (int i = 0; i < 1024; ++i) {
        grid1[byte_positions[i].second][byte_positions[i].first] = '#';
    }
    std::cout << bfs(grid1) << std::endl;

    // Part 2
    std::vector<std::vector<char>> grid2(GRID_SIZE, std::vector<char>(GRID_SIZE, '.'));
    for (const auto& p : byte_positions) {
        grid2[p.second][p.first] = '#';
        if (bfs(grid2) == -1) {
            std::cout << p.first << "," << p.second << std::endl;
            break;
        }
    }

    return 0;
}
