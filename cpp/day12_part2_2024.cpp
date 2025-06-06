
#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <set>
#include <map>
#include <algorithm>

struct Move {
    std::string label;
    int dx, dy;
};

int H, W;
std::vector<Move> moves;
std::vector<std::string> current_graph;

void saveOuter(const std::string& label, std::map<std::string, std::set<std::pair<int, int>>>& side, int x, int y);
bool check_set(const std::set<std::pair<int, int>>& ary_set, int i, int j);
int countOuter(std::map<std::string, std::set<std::pair<int, int>>>& side);

void search(int cx, int cy, const std::string& label, char target,
            int& area, std::set<std::pair<int, int>>& visited,
            std::map<std::string, std::set<std::pair<int, int>>>& side) {

    if (current_graph[cy][cx] != target) {
        if (!label.empty() && visited.find({cx, cy}) == visited.end()) {
            saveOuter(label, side, cx, cy);
        }
        return;
    }

    if (visited.count({cx, cy})) {
        return;
    }

    visited.insert({cx, cy});
    area++;
    current_graph[cy][cx] = '.';

    for (const auto& m : moves) {
        int nx = cx + m.dx;
        int ny = cy + m.dy;

        if (nx < 0 || nx >= W || ny < 0 || ny >= H) {
            saveOuter(m.label, side, nx, ny);
            continue;
        }

        search(nx, ny, m.label, target, area, visited, side);
    }
}

void saveOuter(const std::string& label, std::map<std::string, std::set<std::pair<int, int>>>& side, int x, int y) {
    if (label == "up" || label == "down") {
        side[label].insert({y, x});
    } else {
        side[label].insert({x, y});
    }
}

bool check_set(const std::set<std::pair<int, int>>& ary_set, int i, int j) {
    std::pair<int, int> s1 = {i, j - 1};
    std::pair<int, int> s2 = {i, j + 1};

    return ary_set.count(s1) > 0 || ary_set.count(s2) > 0;
}

int countOuter(std::map<std::string, std::set<std::pair<int, int>>>& side) {
    int outer_segments = 0;
    for (const auto& pair_entry : side) {
        const std::set<std::pair<int, int>>& s = pair_entry.second;

        std::set<std::pair<int, int>> temp_set;

        for (const auto& current_point : s) {
            int i = current_point.first;
            int j = current_point.second;
            if (!check_set(temp_set, i, j)) {
                outer_segments++;
            }
            temp_set.insert(current_point);
        }
    }
    return outer_segments;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::ifstream inputFile("input.txt");
    if (!inputFile.is_open()) {
        return 1;
    }

    std::string line;
    while (std::getline(inputFile, line)) {
        if (line.empty()) {
            continue;
        }
        current_graph.push_back(line);
    }
    inputFile.close();

    H = current_graph.size();
    if (H == 0) {
        std::cout << 0 << std::endl;
        return 0;
    }
    W = current_graph[0].length();

    moves = {
        {"left", -1, 0},
        {"up", 0, -1},
        {"right", 1, 0},
        {"down", 0, 1}
    };

    long long total_sum = 0;

    for (int y = 0; y < H; y++) {
        for (int x = 0; x < W; x++) {
            if (current_graph[y][x] == '.') {
                continue;
            }

            int area = 0;
            char target_char = current_graph[y][x];
            std::set<std::pair<int, int>> visited_component;
            std::map<std::string, std::set<std::pair<int, int>>> side_boundaries;

            search(x, y, "", target_char, area, visited_component, side_boundaries);

            int outer = countOuter(side_boundaries);
            total_sum += static_cast<long long>(area) * outer;
        }
    }

    std::cout << total_sum << std::endl;

    return 0;
}
