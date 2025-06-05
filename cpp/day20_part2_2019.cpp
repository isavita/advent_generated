
#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <map>
#include <set>
#include <queue> // for std::deque
#include <tuple> // for std::tuple

using Pos = std::pair<int, int>;

struct State {
    int x, y, level, steps;

    bool operator<(const State& other) const {
        if (x != other.x) return x < other.x;
        if (y != other.y) return y < other.y;
        return level < other.level;
    }
};

std::vector<std::string> read_input(const std::string& filename) {
    std::vector<std::string> grid;
    std::ifstream file(filename);
    std::string line;
    while (std::getline(file, line)) {
        grid.push_back(line);
    }
    return grid;
}

bool is_letter(char c) {
    return c >= 'A' && c <= 'Z';
}

std::tuple<std::map<std::string, std::vector<Pos>>, int, int> find_portals(const std::vector<std::string>& raw_grid) {
    std::map<std::string, std::vector<Pos>> portals;
    int height = raw_grid.size();
    int width = 0;
    for (const auto& row : raw_grid) {
        if (row.length() > width) {
            width = row.length();
        }
    }

    std::vector<std::string> grid = raw_grid;
    for (auto& row : grid) {
        row.resize(width, ' ');
    }

    for (int y = 0; y < height; ++y) {
        for (int x = 0; x < width; ++x) {
            if (x < width - 1 && is_letter(grid[y][x]) && is_letter(grid[y][x+1])) {
                std::string label;
                label += grid[y][x];
                label += grid[y][x+1];
                
                Pos pos = {-1, -1};
                if (x - 1 >= 0 && grid[y][x-1] == '.') {
                    pos = {x-1, y};
                } else if (x + 2 < width && grid[y][x+2] == '.') {
                    pos = {x+2, y};
                }
                if (pos.first != -1) {
                    portals[label].push_back(pos);
                }
            }
            if (y < height - 1 && is_letter(grid[y][x]) && is_letter(grid[y+1][x])) {
                std::string label;
                label += grid[y][x];
                label += grid[y+1][x];
                
                Pos pos = {-1, -1};
                if (y - 1 >= 0 && grid[y-1][x] == '.') {
                    pos = {x, y-1};
                } else if (y + 2 < height && grid[y+2][x] == '.') {
                    pos = {x, y+2};
                }
                if (pos.first != -1) {
                    portals[label].push_back(pos);
                }
            }
        }
    }
    return std::make_tuple(portals, width, height);
}

bool is_outer(Pos pos, int width, int height) {
    int x = pos.first;
    int y = pos.second;
    return x <= 2 || y <= 2 || x >= width - 3 || y >= height - 3;
}

std::tuple<Pos, Pos, std::map<Pos, std::pair<Pos, bool>>> build_portal_mapping(
    const std::map<std::string, std::vector<Pos>>& portals, int width, int height) {
    
    std::map<Pos, std::pair<Pos, bool>> portal_map;
    Pos start = {-1, -1};
    Pos end = {-1, -1};

    for (const auto& pair : portals) {
        const std::string& label = pair.first;
        const std::vector<Pos>& positions = pair.second;

        if (label == "AA") {
            start = positions[0];
        } else if (label == "ZZ") {
            end = positions[0];
        } else if (positions.size() == 2) {
            Pos a = positions[0];
            Pos b = positions[1];
            
            bool a_outer = is_outer(a, width, height);
            bool b_outer = is_outer(b, width, height);
            
            portal_map[a] = {b, a_outer};
            portal_map[b] = {a, b_outer};
        }
    }
    return std::make_tuple(start, end, portal_map);
}

int bfs_recursive(const std::vector<std::string>& grid, Pos start, Pos end, 
                  const std::map<Pos, std::pair<Pos, bool>>& portal_map, int width, int height) {
    
    std::deque<State> q;
    std::set<State> visited;

    q.push_back({start.first, start.second, 0, 0});
    visited.insert({start.first, start.second, 0, 0});

    std::vector<Pos> directions = {{0, 1}, {0, -1}, {1, 0}, {-1, 0}};

    while (!q.empty()) {
        State current = q.front();
        q.pop_front();

        if (current.x == end.first && current.y == end.second && current.level == 0) {
            return current.steps;
        }

        for (const auto& dir : directions) {
            int nx = current.x + dir.first;
            int ny = current.y + dir.second;

            if (ny < 0 || ny >= grid.size() || nx < 0 || nx >= grid[ny].length()) {
                continue;
            }
            if (grid[ny][nx] != '.') {
                continue;
            }

            State next_state = {nx, ny, current.level, current.steps + 1};
            if (visited.find(next_state) == visited.end()) {
                visited.insert(next_state);
                q.push_back(next_state);
            }
        }

        auto it = portal_map.find({current.x, current.y});
        if (it != portal_map.end()) {
            Pos target_pos = it->second.first;
            bool is_current_portal_outer = it->second.second;

            int new_level = current.level;
            if (is_current_portal_outer) {
                new_level--;
            } else {
                new_level++;
            }

            if (new_level < 0) {
                continue;
            }
            
            State next_portal_state = {target_pos.first, target_pos.second, new_level, current.steps + 1};
            if (visited.find(next_portal_state) == visited.end()) {
                visited.insert(next_portal_state);
                q.push_back(next_portal_state);
            }
        }
    }
    return -1;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::vector<std::string> grid = read_input("input.txt");

    auto [portals, width, height] = find_portals(grid);
    auto [start, end, portal_map] = build_portal_mapping(portals, width, height);

    int result = bfs_recursive(grid, start, end, portal_map, width, height);
    
    std::cout << result << std::endl;

    return 0;
}
