
#include <iostream>
#include <vector>
#include <string>
#include <queue>
#include <map>
#include <tuple>
#include <algorithm> // For std::min

// Directions for BFS (Up, Down, Left, Right)
const int dr[] = {1, -1, 0, 0};
const int dc[] = {0, 0, 1, -1};

// Global grid dimensions and wall information
int H, W;
std::vector<std::vector<bool>> walls;

// Helper to check if coordinates are within grid bounds
bool isValid(int r, int c) {
    return r >= 0 && r < H && c >= 0 && c < W;
}

// Standard BFS function to find shortest paths from a start node to all reachable track cells
std::vector<std::vector<int>> bfs(std::pair<int, int> start_node) {
    std::vector<std::vector<int>> dist(H, std::vector<int>(W, -1));
    std::queue<std::pair<int, int>> q;

    dist[start_node.first][start_node.second] = 0;
    q.push(start_node);

    while (!q.empty()) {
        std::pair<int, int> curr = q.front();
        q.pop();
        int r = curr.first;
        int c = curr.second;

        for (int i = 0; i < 4; ++i) {
            int nr = r + dr[i];
            int nc = c + dc[i];

            // Only move to valid, non-wall cells that haven't been visited
            if (isValid(nr, nc) && !walls[nr][nc] && dist[nr][nc] == -1) {
                dist[nr][nc] = dist[r][c] + 1;
                q.push({nr, nc});
            }
        }
    }
    return dist;
}

void solve() {
    std::vector<std::string> grid;
    std::string line;
    // Read grid from input.txt (redirected stdin)
    while (std::getline(std::cin, line)) {
        grid.push_back(line);
    }

    H = grid.size();
    if (H == 0) { // Handle empty input case
        std::cout << 0 << std::endl;
        return;
    }
    W = grid[0].size();

    walls.assign(H, std::vector<bool>(W, false));
    std::vector<std::pair<int, int>> track_cells;
    std::pair<int, int> S_coords, E_coords;

    // Parse grid to identify S, E, walls, and track cells
    for (int i = 0; i < H; ++i) {
        for (int j = 0; j < W; ++j) {
            if (grid[i][j] == 'S') {
                S_coords = {i, j};
            } else if (grid[i][j] == 'E') {
                E_coords = {i, j};
            }
            if (grid[i][j] == '#') {
                walls[i][j] = true;
            } else {
                track_cells.push_back({i, j});
            }
        }
    }

    // Calculate distances from S and E to all reachable track cells
    std::vector<std::vector<int>> dist_from_s = bfs(S_coords);
    std::vector<std::vector<int>> dist_from_e = bfs(E_coords);

    // If E is not reachable from S, no path exists
    if (dist_from_s[E_coords.first][E_coords.second] == -1) {
        std::cout << 0 << std::endl;
        return;
    }

    int normal_cost = dist_from_s[E_coords.first][E_coords.second];
    // Map to store the minimum cost for each unique cheat path (start_r, start_c, end_r, end_c)
    std::map<std::tuple<int, int, int, int>, int> cheats;

    // Iterate through all possible starting points for a "cheat" path
    for (const auto& start_cell : track_cells) {
        int start_r = start_cell.first;
        int start_c = start_cell.second;

        int sd = dist_from_s[start_r][start_c];
        if (sd == -1) { // If this cell is not reachable from S, it cannot be a cheat start
            continue;
        }

        // BFS for the cheat path, limited to 20 steps
        std::vector<std::vector<int>> dist_c(H, std::vector<int>(W, -1));
        std::queue<std::pair<int, int>> q_cheat;

        dist_c[start_r][start_c] = 0;
        q_cheat.push({start_r, start_c});

        while (!q_cheat.empty()) {
            std::pair<int, int> curr = q_cheat.front();
            q_cheat.pop();
            int r = curr.first;
            int c = curr.second;
            int steps = dist_c[r][c];

            if (steps == 20) { // Limit steps to 20
                continue;
            }

            for (int i = 0; i < 4; ++i) {
                int nr = r + dr[i];
                int nc = c + dc[i];

                // For cheat paths, move to any valid cell, even through walls,
                // matching the Python behavior.
                if (isValid(nr, nc) && dist_c[nr][nc] == -1) {
                    dist_c[nr][nc] = steps + 1;
                    q_cheat.push({nr, nc});
                }
            }
        }

        // After completing the cheat BFS from start_cell,
        // check all possible end points (r, c) for the cheat path
        for (int r = 0; r < H; ++r) {
            for (int c = 0; c < W; ++c) {
                int s = dist_c[r][c]; // Steps for cheat path from start_cell to (r, c)
                
                // Cheat path must have 0 < s <= 20 steps,
                // and its destination (r, c) must be a track cell (not a wall)
                if (s > 0 && s <= 20 && !walls[r][c]) {
                    int ed = dist_from_e[r][c]; // Distance from (r, c) to E
                    if (ed == -1) { // If (r, c) cannot reach E, this cheat path is invalid
                        continue;
                    }
                    
                    int current_cheat_cost = sd + s + ed;

                    // If this cheat path is cheaper than the normal path
                    if (current_cheat_cost < normal_cost) {
                        std::tuple<int, int, int, int> key = {start_r, start_c, r, c};
                        // Store the minimum cost for this specific cheat path
                        if (cheats.find(key) == cheats.end() || current_cheat_cost < cheats[key]) {
                            cheats[key] = current_cheat_cost;
                        }
                    }
                }
            }
        }
    }

    int count = 0;
    // Count how many unique cheat paths save at least 100 steps
    for (const auto& pair : cheats) {
        int cost = pair.second;
        if (normal_cost - cost >= 100) {
            count++;
        }
    }
    std::cout << count << std::endl;
}

int main() {
    // Optimize C++ standard streams for faster I/O
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    // Redirect standard input from "input.txt"
    freopen("input.txt", "r", stdin);

    solve();

    return 0;
}
