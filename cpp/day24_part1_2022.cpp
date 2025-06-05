
#include <iostream>
#include <vector>
#include <string>
#include <set>
#include <deque>
#include <tuple>
#include <numeric> // For std::gcd
#include <fstream> // For file input
#include <cmath>   // For std::abs

long long pos_mod(long long i, long long n) {
    return (i % n + n) % n;
}

std::tuple<std::set<std::pair<int, int>>, std::vector<std::tuple<int, int, char>>, int, int> read_input(const std::string& file_path) {
    std::set<std::pair<int, int>> walls;
    std::vector<std::tuple<int, int, char>> blizzards;
    int height = 0;
    int width = 0;

    std::ifstream file(file_path);
    std::string line;
    int y = 0;
    while (std::getline(file, line)) {
        width = line.length();
        for (int x = 0; x < width; ++x) {
            char char_val = line[x];
            if (char_val == '#') {
                walls.insert({x, y});
            } else if (char_val == '>' || char_val == '<' || char_val == '^' || char_val == 'v') {
                blizzards.emplace_back(x, y, char_val);
            }
        }
        y++;
    }
    height = y;
    return {walls, blizzards, height, width};
}

std::pair<int, int> find_target_point(const std::set<std::pair<int, int>>& walls, int height, int width, bool is_start) {
    int target_y = is_start ? 0 : height - 1;
    for (int x = 0; x < width; ++x) {
        if (walls.find({x, target_y}) == walls.end()) {
            return {x, target_y};
        }
    }
    return {-1, -1};
}

long long compute_lcm(long long a, long long b) {
    if (a == 0 || b == 0) return 0;
    return std::abs(a * b) / std::gcd(a, b);
}

std::vector<std::set<std::pair<int, int>>> precompute_blizzards(
    const std::vector<std::tuple<int, int, char>>& blizzards, int width, int height, long long period) {

    std::vector<std::set<std::pair<int, int>>> blizzard_positions(period);
    int inner_width = width - 2;
    int inner_height = height - 2;

    for (long long t = 0; t < period; ++t) {
        for (const auto& b : blizzards) {
            int x, y;
            char dir;
            std::tie(x, y, dir) = b;

            int new_x = x;
            int new_y = y;

            if (dir == '>') {
                new_x = 1 + pos_mod((long long)x - 1 + t, inner_width);
            } else if (dir == '<') {
                new_x = 1 + pos_mod((long long)x - 1 - t, inner_width);
            } else if (dir == 'v') {
                new_y = 1 + pos_mod((long long)y - 1 + t, inner_height);
            } else if (dir == '^') {
                new_y = 1 + pos_mod((long long)y - 1 - t, inner_height);
            }
            blizzard_positions[t].insert({new_x, new_y});
        }
    }
    return blizzard_positions;
}

int bfs(std::pair<int, int> start, std::pair<int, int> end,
        const std::set<std::pair<int, int>>& walls,
        const std::vector<std::set<std::pair<int, int>>>& blizzard_positions,
        long long period, int width, int height, int initial_time) {

    std::deque<std::tuple<int, int, int>> q;
    std::set<std::tuple<int, int, int>> visited;

    q.emplace_back(start.first, start.second, initial_time);
    visited.insert({start.first, start.second, initial_time % (int)period});

    int directions[5][2] = {{0,0}, {1,0}, {-1,0}, {0,1}, {0,-1}};

    while (!q.empty()) {
        auto [x, y, t] = q.front();
        q.pop_front();

        if (std::make_pair(x, y) == end) {
            return t;
        }

        int next_t = t + 1;
        const auto& blizzards_next = blizzard_positions[next_t % (int)period];

        for (int i = 0; i < 5; ++i) {
            int nx = x + directions[i][0];
            int ny = y + directions[i][1];
            
            if (std::make_pair(nx, ny) == end) {
                return next_t;
            }

            if (std::make_pair(nx, ny) == start) {
                 if (blizzards_next.find({nx, ny}) == blizzards_next.end()) {
                    std::tuple<int, int, int> state = {nx, ny, next_t % (int)period};
                    if (visited.find(state) == visited.end()) {
                        visited.insert(state);
                        q.emplace_back(nx, ny, next_t);
                    }
                }
                continue; 
            }

            if (nx >= 1 && nx < width - 1 && ny >= 1 && ny < height - 1) {
                if (walls.find({nx, ny}) == walls.end() &&
                    blizzards_next.find({nx, ny}) == blizzards_next.end()) {
                    
                    std::tuple<int, int, int> state = {nx, ny, next_t % (int)period};
                    if (visited.find(state) == visited.end()) {
                        visited.insert(state);
                        q.emplace_back(nx, ny, next_t);
                    }
                }
            }
        }
    }
    return -1;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::string input_file = "input.txt";
    auto [walls, blizzards, height, width] = read_input(input_file);

    std::pair<int, int> start_pos = find_target_point(walls, height, width, true);
    std::pair<int, int> end_pos = find_target_point(walls, height, width, false);

    long long period = compute_lcm(width - 2, height - 2);
    std::vector<std::set<std::pair<int, int>>> blizzard_configs = precompute_blizzards(blizzards, width, height, period);
    
    int minutes = bfs(start_pos, end_pos, walls, blizzard_configs, period, width, height, 0);
    std::cout << minutes << std::endl;

    return 0;
}
