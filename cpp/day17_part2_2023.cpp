
#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <queue>
#include <map>
#include <tuple>
#include <functional> // For std::greater

enum Direction { N, E, S, W };

const int dx[] = {0, 1, 0, -1}; // N, E, S, W corresponding to dy
const int dy[] = {-1, 0, 1, 0}; // N, E, S, W corresponding to dx

int get_turned_direction_idx(int current_dir_idx, char turn) {
    if (turn == 'L') {
        return (current_dir_idx - 1 + 4) % 4;
    } else { // 'R'
        return (current_dir_idx + 1) % 4;
    }
}

int char_to_dir_idx(char c) {
    if (c == 'N') return N;
    if (c == 'E') return E;
    if (c == 'S') return S;
    if (c == 'W') return W;
    return -1;
}

std::vector<std::vector<int>> read_input(const std::string& file_path) {
    std::vector<std::vector<int>> grid;
    std::ifstream file(file_path);
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

int dijkstra(const std::vector<std::vector<int>>& grid, int part) {
    int height = grid.size();
    int width = grid[0].size();
    int end_x = width - 1;
    int end_y = height - 1;

    std::priority_queue<std::tuple<int, int, int, int, int>,
                        std::vector<std::tuple<int, int, int, int, int>>,
                        std::greater<std::tuple<int, int, int, int, int>>> pq;

    std::map<std::tuple<int, int, int, int>, int> visited;

    // Initial states: From (0,0), you can start by moving East or South.
    // The cost of (0,0) itself is 0. The cost is accumulated upon entering a new cell.
    // (heat_loss, x, y, direction_idx, steps_in_current_direction)
    pq.push({0, 0, 0, char_to_dir_idx('E'), 0});
    pq.push({0, 0, 0, char_to_dir_idx('S'), 0});

    int max_steps, min_steps_before_turn;
    if (part == 1) {
        max_steps = 3;
        min_steps_before_turn = 1;
    } else { // part == 2
        max_steps = 10;
        min_steps_before_turn = 4;
    }

    while (!pq.empty()) {
        auto [total_heat_loss, x, y, direction_idx, steps] = pq.top();
        pq.pop();

        std::tuple<int, int, int, int> state_key = {x, y, direction_idx, steps};
        if (visited.count(state_key) && visited[state_key] <= total_heat_loss) {
            continue;
        }
        visited[state_key] = total_heat_loss;

        // Destination check
        if (x == end_x && y == end_y) {
            if (part == 2 && steps < min_steps_before_turn) {
                continue; // For Part 2, must meet min_steps_before_turn at destination
            }
            return total_heat_loss;
        }

        // Option 1: Continue in the same direction
        if (steps < max_steps) {
            int nx = x + dx[direction_idx];
            int ny = y + dy[direction_idx];
            int new_steps = steps + 1;

            if (nx >= 0 && nx < width && ny >= 0 && ny < height) {
                int new_heat_loss = total_heat_loss + grid[ny][nx];
                pq.push({new_heat_loss, nx, ny, direction_idx, new_steps});
            }
        }

        // Option 2: Turn left or right
        // Cannot turn if steps < min_steps_before_turn
        // Note: From (0,0), initial steps are 0, so turning is not allowed immediately.
        // This means the first movement must be straight in the initial direction.
        if (steps >= min_steps_before_turn) {
            for (char turn_char : {'L', 'R'}) {
                int new_direction_idx = get_turned_direction_idx(direction_idx, turn_char);
                int nx = x + dx[new_direction_idx];
                int ny = y + dy[new_direction_idx];
                int new_steps = 1; // Reset steps after a turn

                if (nx >= 0 && nx < width && ny >= 0 && ny < height) {
                    int new_heat_loss = total_heat_loss + grid[ny][nx];
                    pq.push({new_heat_loss, nx, ny, new_direction_idx, new_steps});
                }
            }
        }
    }
    return -1;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::vector<std::vector<int>> grid = read_input("input.txt");

    int least_heat_loss_part_one = dijkstra(grid, 1);
    std::cout << least_heat_loss_part_one << std::endl;

    int least_heat_loss_part_two = dijkstra(grid, 2);
    std::cout << least_heat_loss_part_two << std::endl;

    return 0;
}
