
#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <queue>
#include <tuple>
#include <climits>

struct State {
    int y, x, dir, steps;
};

struct PQItem {
    int priority;
    int cost;
    State state;

    bool operator>(const PQItem& other) const {
        return priority > other.priority;
    }
};

int heuristic(int y, int x, int h, int w) {
    return (h - 1 - y) + (w - 1 - x);
}

int solve(const std::vector<std::vector<int>>& grid) {
    int height = grid.size();
    if (height == 0) return 0;
    int width = grid[0].size();
    if (width == 0) return 0;

    const int min_straight = 0;
    const int max_straight = 3;

    std::priority_queue<PQItem, std::vector<PQItem>, std::greater<PQItem>> pq;

    std::vector<std::vector<std::vector<std::vector<int>>>> min_costs(
        height, std::vector<std::vector<std::vector<int>>>(
            width, std::vector<std::vector<int>>(
                4, std::vector<int>(max_straight + 1, INT_MAX)
            )
        )
    );

    int dy[] = {0, 1, 0, -1}; // E, S, W, N
    int dx[] = {1, 0, -1, 0};

    if (width > 1) {
        int c = grid[0][1];
        State s = {0, 1, 0, 1};
        min_costs[0][1][0][1] = c;
        pq.push({c + heuristic(0, 1, height, width), c, s});
    }
    if (height > 1) {
        int c = grid[1][0];
        State s = {1, 0, 1, 1};
        min_costs[1][0][1][1] = c;
        pq.push({c + heuristic(1, 0, height, width), c, s});
    }

    while (!pq.empty()) {
        auto [priority, cost, state] = pq.top();
        pq.pop();

        if (cost > min_costs[state.y][state.x][state.dir][state.steps]) {
            continue;
        }

        if (state.y == height - 1 && state.x == width - 1) {
            return cost;
        }

        for (int next_dir = 0; next_dir < 4; ++next_dir) {
            if (next_dir == (state.dir + 2) % 4) continue;

            bool is_turning = (next_dir != state.dir);

            if (is_turning && state.steps < min_straight) continue;

            int next_steps = is_turning ? 1 : state.steps + 1;

            if (next_steps > max_straight) continue;
            
            int next_y = state.y + dy[next_dir];
            int next_x = state.x + dx[next_dir];

            if (next_y >= 0 && next_y < height && next_x >= 0 && next_x < width) {
                int new_cost = cost + grid[next_y][next_x];
                if (new_cost < min_costs[next_y][next_x][next_dir][next_steps]) {
                    min_costs[next_y][next_x][next_dir][next_steps] = new_cost;
                    State next_state = {next_y, next_x, next_dir, next_steps};
                    pq.push({new_cost + heuristic(next_y, next_x, height, width), new_cost, next_state});
                }
            }
        }
    }

    return -1;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::ifstream file("input.txt");
    if (!file.is_open()) {
        return 1;
    }

    std::vector<std::vector<int>> grid;
    std::string line;

    while (std::getline(file, line)) {
        if (!line.empty()) {
            std::vector<int> row;
            row.reserve(line.length());
            for (char c : line) {
                row.push_back(c - '0');
            }
            grid.push_back(row);
        }
    }
    file.close();

    std::cout << solve(grid) << std::endl;

    return 0;
}
