
#include <fstream>
#include <iostream>
#include <queue>
#include <string>
#include <tuple>

int main() {
    const int GRID_SIZE = 71;
    bool corrupted[GRID_SIZE][GRID_SIZE] = {false};
    bool visited[GRID_SIZE][GRID_SIZE] = {false};

    std::ifstream inputFile("input.txt");
    if (!inputFile.is_open()) {
        return 1;
    }

    std::string line;
    int line_count = 0;
    while (std::getline(inputFile, line) && line_count < 1024) {
        size_t comma_pos = line.find(',');
        if (comma_pos == std::string::npos) continue;

        int x = std::stoi(line.substr(0, comma_pos));
        int y = std::stoi(line.substr(comma_pos + 1));

        if (x >= 0 && x < GRID_SIZE && y >= 0 && y < GRID_SIZE) {
            corrupted[x][y] = true;
        }
        line_count++;
    }
    inputFile.close();

    std::queue<std::tuple<int, int, int>> q;
    q.push({0, 0, 0});
    visited[0][0] = true;

    int dr[] = {0, 0, 1, -1};
    int dc[] = {1, -1, 0, 0};

    while (!q.empty()) {
        auto [x, y, steps] = q.front();
        q.pop();

        if (x == GRID_SIZE - 1 && y == GRID_SIZE - 1) {
            std::cout << steps << std::endl;
            return 0;
        }

        for (int i = 0; i < 4; ++i) {
            int nx = x + dr[i];
            int ny = y + dc[i];

            if (nx >= 0 && nx < GRID_SIZE && ny >= 0 && ny < GRID_SIZE &&
                !corrupted[nx][ny] && !visited[nx][ny]) {
                q.push({nx, ny, steps + 1});
                visited[nx][ny] = true;
            }
        }
    }

    return 0;
}
