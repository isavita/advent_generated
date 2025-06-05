
#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <sstream>
#include <map>
#include <queue>
#include <limits>

struct Point {
    int x, y;

    bool operator<(const Point& other) const {
        if (x != other.x) {
            return x < other.x;
        }
        return y < other.y;
    }

    bool operator==(const Point& other) const {
        return x == other.x && y == other.y;
    }

    bool operator!=(const Point& other) const {
        return !(*this == other);
    }
};

struct Node {
    int used;
    int avail;
};

int max_x = 0;
int max_y = 0;

const std::vector<Point> Neighbors4 = {
    {0, 1}, {0, -1}, {1, 0}, {-1, 0}
};

Point find_hole(const std::vector<std::vector<Node>>& grid) {
    for (int y = 0; y <= max_y; ++y) {
        for (int x = 0; x <= max_x; ++x) {
            if (grid[x][y].used == 0) {
                return {x, y};
            }
        }
    }
    return {-1, -1};
}

int moves(const std::vector<std::vector<Node>>& grid, Point goal_data_pos, Point from_pos, Point to_pos) {
    std::queue<std::pair<Point, int>> q;
    std::vector<std::vector<int>> dist(max_x + 1, std::vector<int>(max_y + 1, -1));

    q.push({from_pos, 0});
    dist[from_pos.x][from_pos.y] = 0;

    while (!q.empty()) {
        Point curr_p = q.front().first;
        int curr_d = q.front().second;
        q.pop();

        if (curr_p == to_pos) {
            return curr_d;
        }

        for (const auto& n_offset : Neighbors4) {
            Point next_p = {curr_p.x + n_offset.x, curr_p.y + n_offset.y};

            if (next_p.x < 0 || next_p.y < 0 || next_p.x > max_x || next_p.y > max_y) {
                continue;
            }

            if (grid[next_p.x][next_p.y].used > 400) {
                continue;
            }

            if (next_p == goal_data_pos) {
                continue;
            }

            if (dist[next_p.x][next_p.y] == -1) {
                dist[next_p.x][next_p.y] = curr_d + 1;
                q.push({next_p, curr_d + 1});
            }
        }
    }
    return std::numeric_limits<int>::max();
}

long long minmoves(const std::vector<std::vector<Node>>& grid) {
    Point goal_data_pos = {max_x, 0};
    Point hole_pos = find_hole(grid);

    long long total_moves = moves(grid, goal_data_pos, hole_pos, {max_x - 1, 0});

    hole_pos = {max_x - 1, 0};

    total_moves += 1;
    goal_data_pos = {max_x - 1, 0};
    hole_pos = {max_x, 0};

    for (int i = 0; i < max_x - 1; ++i) {
        int cycle_moves = moves(grid, goal_data_pos, hole_pos, {goal_data_pos.x - 1, goal_data_pos.y});
        total_moves += cycle_moves;

        hole_pos = {goal_data_pos.x - 1, goal_data_pos.y};

        total_moves += 1;

        Point old_goal_data_pos = goal_data_pos;
        goal_data_pos = hole_pos;
        hole_pos = old_goal_data_pos;
    }

    return total_moves;
}

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        return 1;
    }

    std::string line;
    std::getline(file, line);
    std::getline(file, line);

    std::map<Point, Node> temp_nodes_map;

    while (std::getline(file, line)) {
        std::stringstream ss(line);
        std::string s;
        std::vector<std::string> fields;
        while (ss >> s) {
            fields.push_back(s);
        }

        if (fields.size() < 5) continue;

        std::string node_name = fields[0];
        size_t x_pos = node_name.find("-x");
        size_t y_pos = node_name.find("-y");

        int x_coord = std::stoi(node_name.substr(x_pos + 2, y_pos - (x_pos + 2)));
        int y_coord = std::stoi(node_name.substr(y_pos + 2));

        Point p = {x_coord, y_coord};

        int used = std::stoi(fields[2].substr(0, fields[2].length() - 1));
        int avail = std::stoi(fields[3].substr(0, fields[3].length() - 1));

        temp_nodes_map[p] = {used, avail};

        if (p.x > max_x) max_x = p.x;
        if (p.y > max_y) max_y = p.y;
    }
    file.close();

    std::vector<std::vector<Node>> grid(max_x + 1, std::vector<Node>(max_y + 1));
    for (const auto& pair : temp_nodes_map) {
        grid[pair.first.x][pair.first.y] = pair.second;
    }

    std::cout << minmoves(grid) << std::endl;

    return 0;
}
