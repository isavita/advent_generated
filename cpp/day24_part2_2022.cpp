
#include <iostream>
#include <vector>
#include <string>
#include <set>
#include <deque>
#include <numeric> // For std::gcd
#include <fstream>
#include <algorithm> // For std::abs
#include <tuple>     // For std::tuple and std::tie

struct Blizzard {
    int x, y;
    char dir;
};

struct Point {
    int x, y;
    bool operator<(const Point& other) const {
        if (x != other.x) return x < other.x;
        return y < other.y;
    }
    bool operator==(const Point& other) const {
        return x == other.x && y == other.y;
    }
};

struct State {
    int x, y;
    int time_mod_period;
    bool operator<(const State& other) const {
        if (x != other.x) return x < other.x;
        if (y != other.y) return y < other.y;
        return time_mod_period < other.time_mod_period;
    }
};

long long lcm(long long a, long long b) {
    if (a == 0 || b == 0) return 0;
    return std::abs(a * b) / std::gcd(a, b);
}

std::tuple<std::set<Point>, std::vector<Blizzard>, int, int> read_input(const std::string& file_path) {
    std::set<Point> walls;
    std::vector<Blizzard> blizzards;
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
                blizzards.push_back({x, y, char_val});
            }
        }
        y++;
    }
    height = y;
    return std::make_tuple(walls, blizzards, height, width);
}

std::pair<Point, Point> find_start_end(const std::set<Point>& walls, int height, int width) {
    Point start = {-1, -1};
    Point end = {-1, -1};

    for (int x = 0; x < width; ++x) {
        if (walls.find({x, 0}) == walls.end()) {
            start = {x, 0};
            break;
        }
    }
    for (int x = 0; x < width; ++x) {
        if (walls.find({x, height - 1}) == walls.end()) {
            end = {x, height - 1};
            break;
        }
    }
    return {start, end};
}

long long compute_period(int width_inner, int height_inner) {
    return lcm(width_inner, height_inner);
}

std::vector<std::vector<std::vector<bool>>> precompute_blizzards(const std::vector<Blizzard>& blizzards, int width, int height, long long period) {
    std::vector<std::vector<std::vector<bool>>> blizzard_grids(period, std::vector<std::vector<bool>>(height, std::vector<bool>(width, false)));
    
    int inner_width = width - 2;
    int inner_height = height - 2;

    for (long long t = 0; t < period; ++t) {
        for (const auto& b : blizzards) {
            int x = b.x;
            int y = b.y;
            char dir = b.dir;

            int new_x, new_y;
            if (dir == '>') {
                new_x = 1 + ((x - 1 + t) % inner_width);
                new_y = y;
            } else if (dir == '<') {
                long long temp_x = (long long)x - 1 - t;
                new_x = 1 + ((temp_x % inner_width + inner_width) % inner_width);
                new_y = y;
            } else if (dir == 'v') {
                new_x = x;
                new_y = 1 + ((y - 1 + t) % inner_height);
            } else if (dir == '^') {
                long long temp_y = (long long)y - 1 - t;
                new_x = x;
                new_y = 1 + ((temp_y % inner_height + inner_height) % inner_height);
            }
            blizzard_grids[t][new_y][new_x] = true;
        }
    }
    return blizzard_grids;
}

int bfs(Point start, Point end, const std::set<Point>& walls, const std::vector<std::vector<std::vector<bool>>>& blizzard_positions, long long period, int width, int height, int start_time) {
    std::deque<std::tuple<int, int, int>> q;
    std::set<State> visited;

    q.push_back({start.x, start.y, start_time});
    visited.insert({start.x, start.y, start_time % (int)period});

    int directions[5][2] = { {0,0}, {1,0}, {-1,0}, {0,1}, {0,-1} };

    while (!q.empty()) {
        auto [x, y, t] = q.front();
        q.pop_front();

        if (x == end.x && y == end.y) {
            return t;
        }

        int next_t = t + 1;
        int next_t_mod_period = next_t % (int)period;

        for (int i = 0; i < 5; ++i) {
            int nx = x + directions[i][0];
            int ny = y + directions[i][1];

            if (nx == end.x && ny == end.y) {
                if (blizzard_positions[next_t_mod_period][ny][nx]) {
                    continue;
                }
                return next_t;
            }
            if (nx == start.x && ny == start.y) {
                if (blizzard_positions[next_t_mod_period][ny][nx]) {
                    continue;
                }
                State next_state_visited = {nx, ny, next_t_mod_period};
                if (visited.find(next_state_visited) == visited.end()) {
                    visited.insert(next_state_visited);
                    q.push_back({nx, ny, next_t});
                }
                continue;
            }

            if (nx >= 1 && nx < width - 1 && ny >= 1 && ny < height - 1) {
                if (walls.find({nx, ny}) != walls.end() || blizzard_positions[next_t_mod_period][ny][nx]) {
                    continue;
                }
                State next_state_visited = {nx, ny, next_t_mod_period};
                if (visited.find(next_state_visited) == visited.end()) {
                    visited.insert(next_state_visited);
                    q.push_back({nx, ny, next_t});
                }
            }
        }
    }
    return -1;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    const std::string input_file = "input.txt";
    
    std::set<Point> walls;
    std::vector<Blizzard> blizzards;
    int height, width;

    std::tie(walls, blizzards, height, width) = read_input(input_file);
    
    Point start, end;
    std::tie(start, end) = find_start_end(walls, height, width);
    
    long long period = compute_period(width - 2, height - 2);
    std::vector<std::vector<std::vector<bool>>> blizzard_positions = precompute_blizzards(blizzards, width, height, period);
    
    int time1 = bfs(start, end, walls, blizzard_positions, period, width, height, 0);
    int time2 = bfs(end, start, walls, blizzard_positions, period, width, height, time1);
    int time3 = bfs(start, end, walls, blizzard_positions, period, width, height, time2);
    
    std::cout << time3 << std::endl;

    return 0;
}
