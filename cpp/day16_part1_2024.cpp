
#include <iostream>
#include <vector>
#include <string>
#include <queue>
#include <fstream>

struct State {
    int cost;
    int r, c;
    int dir; // 0: East, 1: South, 2: West, 3: North

    bool operator>(const State& other) const {
        return cost > other.cost;
    }
};

const int dr[] = {0, 1, 0, -1};
const int dc[] = {1, 0, -1, 0};

void solve() {
    std::vector<std::string> grid;
    std::string line;
    std::ifstream inputFile("input.txt");

    if (!inputFile.is_open()) {
        return;
    }

    while (std::getline(inputFile, line)) {
        grid.push_back(line);
    }
    inputFile.close();

    int rows = grid.size();
    int cols = grid[0].length();

    int start_row = -1, start_col = -1;
    int end_row = -1, end_col = -1;

    for (int r = 0; r < rows; ++r) {
        for (int c = 0; c < cols; ++c) {
            if (grid[r][c] == 'S') {
                start_row = r;
                start_col = c;
            } else if (grid[r][c] == 'E') {
                end_row = r;
                end_col = c;
            }
        }
    }

    std::vector<std::vector<std::vector<bool>>> visited(rows, std::vector<std::vector<bool>>(cols, std::vector<bool>(4, false)));

    std::priority_queue<State, std::vector<State>, std::greater<State>> pq;

    pq.push({0, start_row, start_col, 0}); 

    while (!pq.empty()) {
        State current = pq.top();
        pq.pop();

        int cost = current.cost;
        int r = current.r;
        int c = current.c;
        int dir = current.dir;

        if (visited[r][c][dir]) {
            continue;
        }
        visited[r][c][dir] = true;

        if (r == end_row && c == end_col) {
            std::cout << cost << std::endl;
            return;
        }

        // Move forward
        int nr = r + dr[dir];
        int nc = c + dc[dir];
        if (nr >= 0 && nr < rows && nc >= 0 && nc < cols && grid[nr][nc] != '#') {
            pq.push({cost + 1, nr, nc, dir});
        }

        // Rotate clockwise
        int new_dir_clockwise = (dir + 1) % 4;
        pq.push({cost + 1000, r, c, new_dir_clockwise});

        // Rotate counter-clockwise
        int new_dir_counter_clockwise = (dir - 1 + 4) % 4;
        pq.push({cost + 1000, r, c, new_dir_counter_clockwise});
    }
}

int main() {
    solve();
    return 0;
}

