
#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <map>
#include <set>
#include <queue>
#include <utility>

struct State {
    int x, y, steps;
};

std::vector<std::string> readMaze(const std::string& filename) {
    std::vector<std::string> maze;
    std::ifstream file(filename);
    std::string line;
    while (std::getline(file, line)) {
        maze.push_back(line);
    }
    return maze;
}

std::pair<std::map<std::string, std::vector<std::pair<int, int>>>,
          std::map<std::pair<int, int>, std::string>>
findPortals(const std::vector<std::string>& maze) {

    int height = maze.size();
    int width = maze[0].size();

    std::map<std::string, std::vector<std::pair<int, int>>> portals;
    std::map<std::pair<int, int>, std::string> portalPositions;

    for (int y = 0; y < height; ++y) {
        for (int x = 0; x < width; ++x) {
            char currentChar = maze[y][x];

            if (currentChar >= 'A' && currentChar <= 'Z') {
                if (x + 1 < width && (maze[y][x+1] >= 'A' && maze[y][x+1] <= 'Z')) {
                    std::string portalName = "";
                    portalName += currentChar;
                    portalName += maze[y][x+1];

                    if (x + 2 < width && maze[y][x+2] == '.') {
                        portals[portalName].push_back({x + 2, y});
                        portalPositions[{x + 2, y}] = portalName;
                    }
                    else if (x - 1 >= 0 && maze[y][x-1] == '.') {
                        portals[portalName].push_back({x - 1, y});
                        portalPositions[{x - 1, y}] = portalName;
                    }
                }
                
                if (y + 1 < height && (maze[y+1][x] >= 'A' && maze[y+1][x] <= 'Z')) {
                    std::string portalName = "";
                    portalName += currentChar;
                    portalName += maze[y+1][x];

                    if (y + 2 < height && maze[y+2][x] == '.') {
                        portals[portalName].push_back({x, y + 2});
                        portalPositions[{x, y + 2}] = portalName;
                    }
                    else if (y - 1 >= 0 && maze[y-1][x] == '.') {
                        portals[portalName].push_back({x, y - 1});
                        portalPositions[{x, y - 1}] = portalName;
                    }
                }
            }
        }
    }
    return {portals, portalPositions};
}

int bfs(const std::vector<std::string>& maze,
        const std::map<std::string, std::vector<std::pair<int, int>>>& portals,
        const std::map<std::pair<int, int>, std::string>& portalPositions,
        std::pair<int, int> start, std::pair<int, int> end) {

    std::queue<State> q;
    q.push({start.first, start.second, 0});

    std::set<std::pair<int, int>> visited;
    visited.insert(start);

    int dx[] = {-1, 1, 0, 0};
    int dy[] = {0, 0, -1, 1};

    int height = maze.size();
    int width = maze[0].size();

    while (!q.empty()) {
        State current = q.front();
        q.pop();

        int x = current.x;
        int y = current.y;
        int steps = current.steps;

        for (int i = 0; i < 4; ++i) {
            int nx = x + dx[i];
            int ny = y + dy[i];

            if (nx >= 0 && nx < width && ny >= 0 && ny < height && maze[ny][nx] == '.') {
                if (nx == end.first && ny == end.second) {
                    return steps + 1;
                }
                if (visited.find({nx, ny}) == visited.end()) {
                    visited.insert({nx, ny});
                    q.push({nx, ny, steps + 1});
                }
            }
        }

        auto it = portalPositions.find({x, y});
        if (it != portalPositions.end()) {
            std::string portalName = it->second;
            const std::vector<std::pair<int, int>>& exits = portals.at(portalName);

            for (const auto& exit_pos : exits) {
                if (exit_pos.first == x && exit_pos.second == y) {
                    continue;
                }
                if (visited.find(exit_pos) == visited.end()) {
                    visited.insert(exit_pos);
                    q.push({exit_pos.first, exit_pos.second, steps + 1});
                }
            }
        }
    }
    return -1;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::vector<std::string> maze = readMaze("input.txt");
    
    auto portal_data = findPortals(maze);
    const auto& portals = portal_data.first;
    const auto& portalPositions = portal_data.second;

    std::pair<int, int> start_pos = portals.at("AA")[0];
    std::pair<int, int> end_pos = portals.at("ZZ")[0];
    
    int result = bfs(maze, portals, portalPositions, start_pos, end_pos);
    std::cout << result << std::endl;

    return 0;
}
