
#include <iostream>
#include <vector>
#include <string>
#include <queue>
#include <map>
#include <unordered_set>
#include <fstream>
#include <cctype> // For std::islower, std::isupper, std::tolower

struct Point {
    int x, y;

    bool operator==(const Point& other) const {
        return x == other.x && y == other.y;
    }
};

struct State {
    Point pos;
    int keys;

    bool operator==(const State& other) const {
        return pos == other.pos && keys == other.keys;
    }
};

struct StateHash {
    size_t operator()(const State& s) const {
        size_t h1 = std::hash<int>()(s.pos.x);
        size_t h2 = std::hash<int>()(s.pos.y);
        size_t h3 = std::hash<int>()(s.keys);
        return h1 ^ (h2 << 1) ^ (h3 << 2);
    }
};

int find_shortest_path(const std::vector<std::string>& grid, Point start, const std::map<char, int>& key_map) {
    std::vector<Point> dirs = {{0, -1}, {-1, 0}, {0, 1}, {1, 0}};
    std::unordered_set<State, StateHash> visited;
    std::deque<State> q;

    q.push_back({start, 0});
    visited.insert({start, 0});

    int steps = 0;
    int target_keys = (1 << key_map.size()) - 1;

    int grid_width = grid[0].length();
    int grid_height = grid.size();

    while (!q.empty()) {
        int level_size = q.size();
        for (int i = 0; i < level_size; ++i) {
            State current = q.front();
            q.pop_front();

            if (current.keys == target_keys) {
                return steps;
            }

            for (const auto& d : dirs) {
                Point next_pos = {current.pos.x + d.x, current.pos.y + d.y};

                if (next_pos.x < 0 || next_pos.y < 0 || next_pos.x >= grid_width || next_pos.y >= grid_height) {
                    continue;
                }

                char cell_char = grid[next_pos.y][next_pos.x];

                if (cell_char == '#') {
                    continue;
                }

                if (std::isupper(cell_char)) {
                    char required_key_char = static_cast<char>(std::tolower(static_cast<unsigned char>(cell_char)));
                    if (key_map.count(required_key_char) == 0) { // Door has no corresponding key defined.
                        continue;
                    }
                    int required_key_bit = key_map.at(required_key_char);
                    if (!(current.keys & (1 << required_key_bit))) {
                        continue;
                    }
                }

                State next_state = {next_pos, current.keys};

                if (std::islower(cell_char)) {
                    if (key_map.count(cell_char) > 0) {
                        next_state.keys |= (1 << key_map.at(cell_char));
                    }
                }

                if (visited.find(next_state) == visited.end()) {
                    visited.insert(next_state);
                    q.push_back(next_state);
                }
            }
        }
        steps++;
    }

    return -1;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening input.txt" << std::endl;
        return 1;
    }

    std::vector<std::string> grid;
    Point start_pos = {-1, -1};
    std::map<char, int> key_map;
    int key_counter = 0;
    std::string line;
    int y = 0;

    while (std::getline(file, line)) {
        grid.push_back(line);
        for (int x = 0; x < line.length(); ++x) {
            char c = line[x];
            if (c == '@') {
                start_pos = {x, y};
            } else if (std::islower(static_cast<unsigned char>(c))) {
                if (key_map.find(c) == key_map.end()) {
                    key_map[c] = key_counter++;
                }
            }
        }
        y++;
    }
    file.close();

    std::cout << find_shortest_path(grid, start_pos, key_map) << std::endl;

    return 0;
}
