
#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <queue>
#include <map>
#include <algorithm>
#include <limits>

std::vector<std::string> layout;
int R, C;

int dr[] = {-1, 1, 0, 0};
int dc[] = {0, 0, -1, 1};

void read_map(const std::string& filename) {
    std::ifstream file(filename);
    std::string line;
    while (std::getline(file, line)) {
        layout.push_back(line);
    }
    R = layout.size();
    if (R > 0) {
        C = layout[0].length();
    } else {
        C = 0;
    }
}

std::map<char, std::pair<int, int>> find_interests() {
    std::map<char, std::pair<int, int>> interests;
    for (int r = 0; r < R; ++r) {
        for (int c = 0; c < C; ++c) {
            if (isdigit(layout[r][c])) {
                interests[layout[r][c]] = {r, c};
            }
        }
    }
    return interests;
}

std::map<char, int> bfs(std::pair<int, int> start_coord) {
    std::map<char, int> distances_from_start;
    std::queue<std::pair<int, int>> q;
    std::vector<std::vector<int>> dist(R, std::vector<int>(C, -1));

    q.push(start_coord);
    dist[start_coord.first][start_coord.second] = 0;

    if (isdigit(layout[start_coord.first][start_coord.second])) {
        distances_from_start[layout[start_coord.first][start_coord.second]] = 0;
    }

    while (!q.empty()) {
        std::pair<int, int> curr = q.front();
        q.pop();
        int r = curr.first;
        int c = curr.second;

        if (isdigit(layout[r][c])) {
            distances_from_start[layout[r][c]] = dist[r][c];
        }

        for (int i = 0; i < 4; ++i) {
            int nr = r + dr[i];
            int nc = c + dc[i];

            if (nr >= 0 && nr < R && nc >= 0 && nc < C && layout[nr][nc] != '#' && dist[nr][nc] == -1) {
                dist[nr][nc] = dist[r][c] + 1;
                q.push({nr, nc});
            }
        }
    }
    return distances_from_start;
}

int solve_tsp_with_return(const std::map<char, std::map<char, int>>& all_distances) {
    char start_node_char = '0';
    
    if (all_distances.find(start_node_char) == all_distances.end()) {
        return std::numeric_limits<int>::max();
    }

    std::vector<char> other_nodes;
    for (const auto& pair : all_distances) {
        if (pair.first != start_node_char) {
            other_nodes.push_back(pair.first);
        }
    }

    if (other_nodes.empty()) {
        return 0;
    }

    std::sort(other_nodes.begin(), other_nodes.end());

    long long min_path_length = std::numeric_limits<long long>::max();

    do {
        long long current_path_length = 0;
        char current_char = start_node_char;

        auto it_first = all_distances.at(current_char).find(other_nodes[0]);
        if (it_first == all_distances.at(current_char).end()) {
            current_path_length = std::numeric_limits<long long>::max();
        } else {
            current_path_length += it_first->second;
            current_char = other_nodes[0];
        }
        
        if (current_path_length == std::numeric_limits<long long>::max()) {
            continue;
        }

        for (size_t i = 1; i < other_nodes.size(); ++i) {
            auto it = all_distances.at(current_char).find(other_nodes[i]);
            if (it == all_distances.at(current_char).end()) {
                current_path_length = std::numeric_limits<long long>::max();
                break;
            }
            current_path_length += it->second;
            current_char = other_nodes[i];
        }

        if (current_path_length == std::numeric_limits<long long>::max()) {
            continue;
        }

        auto it_return = all_distances.at(current_char).find(start_node_char);
        if (it_return == all_distances.at(current_char).end()) {
            current_path_length = std::numeric_limits<long long>::max();
        } else {
            current_path_length += it_return->second;
        }

        min_path_length = std::min(min_path_length, current_path_length);

    } while (std::next_permutation(other_nodes.begin(), other_nodes.end()));

    return static_cast<int>(min_path_length);
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    read_map("input.txt");
    std::map<char, std::pair<int, int>> interests = find_interests();

    std::map<char, std::map<char, int>> all_pairs_distances;

    for (const auto& entry : interests) {
        char start_char = entry.first;
        std::pair<int, int> start_coord = entry.second;
        all_pairs_distances[start_char] = bfs(start_coord);
    }
    
    int answer = solve_tsp_with_return(all_pairs_distances);
    std::cout << answer << std::endl;

    return 0;
}
