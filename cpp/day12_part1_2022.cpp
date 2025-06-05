
#include <iostream>
#include <vector>
#include <string>
#include <queue>
#include <map>
#include <limits>
#include <fstream>
#include <algorithm>

std::map<std::pair<int, int>, int> djikstra(
    const std::vector<std::string>& grid_chars,
    std::pair<int, int> end_p,
    int max_y, int max_x
) {
    std::priority_queue<
        std::pair<int, std::pair<int, int>>,
        std::vector<std::pair<int, std::pair<int, int>>>,
        std::greater<std::pair<int, std::pair<int, int>>>
    > pq;

    std::map<std::pair<int, int>, int> dist;

    pq.push({0, end_p});
    dist[end_p] = 0;

    int dx[] = {0, 0, 1, -1};
    int dy[] = {1, -1, 0, 0};

    while (!pq.empty()) {
        auto [d, curr] = pq.top();
        pq.pop();

        if (d > dist[curr]) {
            continue;
        }

        for (int i = 0; i < 4; ++i) {
            std::pair<int, int> next = {curr.first + dx[i], curr.second + dy[i]};

            if (next.first < 0 || next.first >= max_x || next.second < 0 || next.second >= max_y) {
                continue;
            }

            char curr_char = grid_chars[curr.second][curr.first];
            char next_char = grid_chars[next.second][next.first];

            if (static_cast<int>(curr_char) - static_cast<int>(next_char) > 1) {
                continue;
            }

            int next_dist = dist[curr] + 1;

            if (dist.find(next) == dist.end() || next_dist < dist[next]) {
                dist[next] = next_dist;
                pq.push({next_dist, next});
            }
        }
    }
    return dist;
}

int main() {
    std::vector<std::string> grid_chars;
    std::pair<int, int> start_p = {-1, -1};
    std::pair<int, int> end_p = {-1, -1};

    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening input.txt" << std::endl;
        return 1;
    }

    std::string line;
    int y = 0;
    int max_x = 0;
    while (std::getline(file, line)) {
        if (line.empty()) continue;
        
        if (y == 0) {
            max_x = line.length();
        }

        for (int x = 0; x < line.length(); ++x) {
            char b = line[x];
            if (b == 'S') {
                start_p = {x, y};
            } else if (b == 'E') {
                end_p = {x, y};
            }
        }
        grid_chars.push_back(line);
        y++;
    }
    file.close();

    int max_y = y;

    grid_chars[start_p.second][start_p.first] = 'a';
    grid_chars[end_p.second][end_p.first] = 'z';

    std::map<std::pair<int, int>, int> dists = djikstra(grid_chars, end_p, max_y, max_x);

    int shortest_path_s_to_e = std::numeric_limits<int>::max();
    if (dists.count(start_p)) {
        shortest_path_s_to_e = dists[start_p];
    }
    std::cout << shortest_path_s_to_e << std::endl;

    return 0;
}
