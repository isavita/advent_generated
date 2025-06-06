
#include <iostream>
#include <vector>
#include <string>
#include <queue>
#include <limits>
#include <functional>
#include <unordered_set>
#include <unordered_map>
#include <fstream>
#include <algorithm>

struct Coord {
    int x, y;

    bool operator==(const Coord& other) const { return x == other.x && y == other.y; }
};

struct CoordHash {
    std::size_t operator()(const Coord& c) const {
        std::size_t h1 = std::hash<int>()(c.x);
        std::size_t h2 = std::hash<int>()(c.y);
        return h1 ^ (h2 << 1);
    }
};

struct Edge {
    Coord start;
    Coord end;
    int weight;
};

struct Graph {
    std::unordered_set<Coord, CoordHash> vertices;
    std::unordered_map<Coord, std::vector<Edge>, CoordHash> edges;
};

const Coord North = {0, -1};
const Coord South = {0, 1};
const Coord West = {-1, 0};
const Coord East = {1, 0};

const char Empty = '.';
const char Wall = '#';
const char NorthSlopes = '^';
const char SouthSlopes = 'v';
const char WestSlopes = '<';
const char EastSlopes = '>';

std::unordered_map<char, Coord> SlopeToDir;

void initSlopeToDir() {
    SlopeToDir[NorthSlopes] = North;
    SlopeToDir[SouthSlopes] = South;
    SlopeToDir[WestSlopes] = West;
    SlopeToDir[EastSlopes] = East;
}

Coord addCoords(Coord c1, Coord c2) {
    return {c1.x + c2.x, c1.y + c2.y};
}

bool isInBounds(const std::vector<std::string>& grid, Coord coord) {
    return coord.x >= 0 && coord.x < grid[0].length() &&
           coord.y >= 0 && coord.y < grid.size();
}

bool isValidNeighborNoSlopes(const std::vector<std::string>& grid, Coord coord, Coord /*dir*/) {
    if (!isInBounds(grid, coord)) {
        return false;
    }
    return grid[coord.y][coord.x] != Wall;
}

bool isValidNeighborWithSlopes(const std::vector<std::string>& grid, Coord coord, Coord dir) {
    if (!isInBounds(grid, coord)) {
        return false;
    }
    char cell = grid[coord.y][coord.x];
    if (cell == Empty) {
        return true;
    }
    if (cell == Wall) {
        return false;
    }
    return SlopeToDir.at(cell).x == dir.x && SlopeToDir.at(cell).y == dir.y;
}

std::vector<Coord> neighbors4(
    const std::vector<std::string>& grid,
    Coord coord,
    std::function<bool(const std::vector<std::string>&, Coord, Coord)> isValidNeighborFunc
) {
    const std::vector<Coord> directions = {North, South, West, East};
    std::vector<Coord> validNeighbors;

    for (const auto& dir : directions) {
        Coord neighbor = addCoords(coord, dir);
        if (isValidNeighborFunc(grid, neighbor, dir)) {
            validNeighbors.push_back(neighbor);
        }
    }
    return validNeighbors;
}

std::vector<Edge> getEdgesBFS(
    const std::vector<std::string>& grid,
    Coord start,
    const std::unordered_set<Coord, CoordHash>& vertices,
    std::function<bool(const std::vector<std::string>&, Coord, Coord)> isValidNeighborFunc
) {
    std::queue<Coord> frontier;
    frontier.push(start);

    std::unordered_set<Coord, CoordHash> reached;
    reached.insert(start);

    std::unordered_map<Coord, int, CoordHash> distances;
    distances[start] = 0;

    std::vector<Edge> result;

    while (!frontier.empty()) {
        Coord current = frontier.front();
        frontier.pop();

        if (vertices.count(current) && !(current == start)) {
            result.push_back({start, current, distances.at(current)});
            continue;
        }

        for (const auto& next : neighbors4(grid, current, isValidNeighborFunc)) {
            if (reached.find(next) == reached.end()) {
                frontier.push(next);
                reached.insert(next);
                distances[next] = distances.at(current) + 1;
            }
        }
    }
    return result;
}

Graph getGraph(
    const std::vector<std::string>& grid,
    Coord start,
    Coord end,
    std::function<bool(const std::vector<std::string>&, Coord, Coord)> isValidNeighborFunc
) {
    Graph graph;
    graph.vertices.insert(start);
    graph.vertices.insert(end);

    for (int y = 0; y < grid.size(); ++y) {
        for (int x = 0; x < grid[0].length(); ++x) {
            Coord coord = {x, y};
            if (grid[y][x] == Wall) continue;
            if (neighbors4(grid, coord, isValidNeighborNoSlopes).size() > 2) {
                graph.vertices.insert(coord);
            }
        }
    }

    for (const auto& vertex : graph.vertices) {
        graph.edges[vertex] = getEdgesBFS(grid, vertex, graph.vertices, isValidNeighborFunc);
    }

    return graph;
}

int getMaxDistanceDFS(
    const Graph& graph,
    Coord current,
    Coord end,
    std::unordered_set<Coord, CoordHash>& seen
) {
    if (current == end) {
        return 0;
    }

    int maxDist = std::numeric_limits<int>::min();

    seen.insert(current);

    auto it = graph.edges.find(current);
    if (it != graph.edges.end()) {
        for (const auto& edge : it->second) {
            if (seen.find(edge.end) == seen.end()) {
                int dist = getMaxDistanceDFS(graph, edge.end, end, seen);
                if (dist != std::numeric_limits<int>::min()) {
                    maxDist = std::max(maxDist, dist + edge.weight);
                }
            }
        }
    }

    seen.erase(current);
    return maxDist;
}

int solve(const std::vector<std::string>& input) {
    Coord start = {1, 0};
    Coord end = {(int)input[0].length() - 2, (int)input.size() - 1};

    initSlopeToDir();

    Graph graph = getGraph(input, start, end, isValidNeighborWithSlopes);

    std::unordered_set<Coord, CoordHash> seen;
    return getMaxDistanceDFS(graph, start, end, seen);
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::vector<std::string> input;
    std::ifstream inputFile("input.txt");
    if (!inputFile.is_open()) {
        std::cerr << "Error opening input.txt" << std::endl;
        return 1;
    }

    std::string line;
    while (std::getline(inputFile, line)) {
        input.push_back(line);
    }
    inputFile.close();

    std::cout << solve(input) << std::endl;

    return 0;
}
