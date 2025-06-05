
#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <sstream>
#include <queue>
#include <map>
#include <limits>
#include <algorithm>

struct BfsNode {
    int r, c, dist;
};

int dr[] = {0, 0, 1, -1};
int dc[] = {-1, 1, 0, 0};

std::string readFile(const std::string& path) {
    std::ifstream file(path);
    std::stringstream buffer;
    buffer << file.rdbuf();
    std::string content = buffer.str();
    
    if (!content.empty() && content.back() == '\n') {
        content.pop_back();
    }
    return content;
}

std::vector<int> bfsGetEdgeWeights(const std::vector<std::string>& grid, int start_r, int start_c, int max_poi_val) {
    int rows = grid.size();
    int cols = grid[0].size();
    std::vector<std::vector<bool>> visited(rows, std::vector<bool>(cols, false));
    std::queue<BfsNode> q;
    std::map<char, int> poiToDistance;

    q.push({start_r, start_c, 0});
    visited[start_r][start_c] = true;

    if (isdigit(grid[start_r][start_c])) {
        poiToDistance[grid[start_r][start_c]] = 0;
    }

    while (!q.empty()) {
        BfsNode front = q.front();
        q.pop();
        
        if (isdigit(grid[front.r][front.c])) {
            poiToDistance[grid[front.r][front.c]] = front.dist;
        }

        for (int i = 0; i < 4; ++i) {
            int nr = front.r + dr[i];
            int nc = front.c + dc[i];

            if (nr >= 0 && nr < rows && nc >= 0 && nc < cols && grid[nr][nc] != '#') {
                if (!visited[nr][nc]) {
                    visited[nr][nc] = true;
                    q.push({nr, nc, front.dist + 1});
                }
            }
        }
    }

    std::vector<int> distances(max_poi_val + 1, -1); 
    for (auto const& pair : poiToDistance) {
        int poi_idx = pair.first - '0';
        distances[poi_idx] = pair.second;
    }
    return distances;
}

int dfs(const std::vector<std::vector<int>>& graph, int current_poi_idx, std::vector<bool>& visited_pois, int visited_count, bool return_to_zero) {
    if (visited_count == graph.size()) {
        if (return_to_zero) {
            return graph[current_poi_idx][0];
        }
        return 0;
    }
    
    int min_distance = std::numeric_limits<int>::max();

    for (int i = 0; i < graph.size(); ++i) {
        if (!visited_pois[i]) {
            if (graph[current_poi_idx][i] == -1) {
                continue; 
            }

            visited_pois[i] = true;
            
            int dist = graph[current_poi_idx][i] + dfs(graph, i, visited_pois, visited_count + 1, return_to_zero);
            min_distance = std::min(min_distance, dist);

            visited_pois[i] = false;
        }
    }

    return min_distance;
}

int cleaningRobot(const std::string& input_content) {
    std::vector<std::string> grid;
    std::stringstream ss(input_content);
    std::string line;
    int max_poi_val = -1;

    while (std::getline(ss, line)) {
        grid.push_back(line);
        for (char c : line) {
            if (isdigit(c)) {
                max_poi_val = std::max(max_poi_val, c - '0');
            }
        }
    }

    if (max_poi_val == -1) {
        return 0;
    }
    
    std::vector<std::pair<int, int>> poi_coords(max_poi_val + 1);
    for (int r = 0; r < grid.size(); ++r) {
        for (int c = 0; c < grid[r].size(); ++c) {
            if (isdigit(grid[r][c])) {
                poi_coords[grid[r][c] - '0'] = {r, c};
            }
        }
    }

    std::vector<std::vector<int>> graph(max_poi_val + 1, std::vector<int>(max_poi_val + 1));
    for (int i = 0; i <= max_poi_val; ++i) {
        std::vector<int> distances_from_i = bfsGetEdgeWeights(grid, poi_coords[i].first, poi_coords[i].second, max_poi_val);
        for (int j = 0; j <= max_poi_val; ++j) {
            graph[i][j] = distances_from_i[j];
        }
    }
    
    std::vector<bool> visited_pois(max_poi_val + 1, false);
    visited_pois[0] = true; 
    return dfs(graph, 0, visited_pois, 1, false);
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::string input_content = readFile("input.txt");
    int result = cleaningRobot(input_content);
    std::cout << result << std::endl;
    return 0;
}
