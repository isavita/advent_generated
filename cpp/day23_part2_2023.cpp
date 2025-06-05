
#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <set>
#include <deque>
#include <algorithm> // For std::max
#include <fstream>   // For file operations

struct Coord {
    int x, y;

    bool operator<(const Coord& other) const {
        if (y != other.y) return y < other.y;
        return x < other.x;
    }

    bool operator==(const Coord& other) const {
        return x == other.x && y == other.y;
    }

    Coord operator+(const Coord& other) const {
        return {x + other.x, y + other.y};
    }
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

// SlopeToDir is not used in the translated Python logic's core pathfinding,
// but included for completeness if its constants were referenced.
// The Python solution provided treats slopes as regular path segments
// for the purpose of graph construction and DFS traversal.
std::map<char, Coord> SlopeToDir;

void initSlopeToDir() {
    SlopeToDir[NorthSlopes] = North;
    SlopeToDir[SouthSlopes] = South;
    SlopeToDir[WestSlopes] = West;
    SlopeToDir[EastSlopes] = East;
}

struct Grid {
    int width;
    int height;
    std::map<Coord, char> data;
};

struct Edge {
    Coord start, end;
    int weight;

    bool operator<(const Edge& other) const {
        if (start < other.start) return true;
        if (other.start < start) return false;
        if (end < other.end) return true;
        if (other.end < end) return false;
        return weight < other.weight;
    }

    bool operator==(const Edge& other) const {
        return start == other.start && end == other.end && weight == other.weight;
    }
};

struct Graph {
    std::set<Coord> vertices;
    std::map<Coord, std::set<Edge>> edges;
};

bool is_in_bounds(const Grid& grid, const Coord& coord) {
    return coord.x >= 0 && coord.x < grid.width && coord.y >= 0 && coord.y < grid.height;
}

bool is_valid_neighbor(const Grid& grid, const Coord& coord, const Coord& dir) {
    if (!is_in_bounds(grid, coord)) {
        return false;
    }
    auto it = grid.data.find(coord);
    if (it != grid.data.end() && it->second == Wall) {
        return false;
    }
    return true;
}

std::vector<Coord> neighbors4(const Grid& grid, const Coord& coord) {
    std::vector<Coord> directions = {North, South, West, East};
    std::vector<Coord> valid_neighbors;
    for (const auto& dir : directions) {
        Coord neighbor = coord + dir;
        if (is_valid_neighbor(grid, neighbor, dir)) {
            valid_neighbors.push_back(neighbor);
        }
    }
    return valid_neighbors;
}

Grid parse_input(const std::vector<std::string>& input_lines) {
    Grid grid;
    grid.height = input_lines.size();
    grid.width = input_lines.empty() ? 0 : input_lines[0].length();

    for (int y = 0; y < grid.height; ++y) {
        for (int x = 0; x < grid.width; ++x) {
            char char_val = input_lines[y][x];
            if (char_val != Empty) {
                grid.data[{x, y}] = char_val;
            }
        }
    }
    return grid;
}

std::set<Edge> get_edges_bfs(const Grid& grid, const Coord& start, const std::set<Coord>& vertices) {
    std::deque<Coord> frontier;
    frontier.push_back(start);
    std::set<Coord> reached;
    reached.insert(start);
    std::map<Coord, int> distances;
    distances[start] = 0;

    std::set<Edge> edges;

    while (!frontier.empty()) {
        Coord current = frontier.front();
        frontier.pop_front();

        if (vertices.count(current) && !(current == start)) {
            edges.insert({start, current, distances[current]});
            continue;
        }

        for (const auto& next : neighbors4(grid, current)) {
            if (!reached.count(next)) {
                frontier.push_back(next);
                reached.insert(next);
                distances[next] = distances[current] + 1;
            }
        }
    }
    return edges;
}

Graph get_graph(const Grid& grid, const Coord& start, const Coord& end) {
    Graph graph;
    graph.vertices.insert(start);
    graph.vertices.insert(end);

    for (int y = 0; y < grid.height; ++y) {
        for (int x = 0; x < grid.width; ++x) {
            Coord coord = {x, y};
            auto it = grid.data.find(coord);
            if (it == grid.data.end() || it->second != Wall) {
                if (neighbors4(grid, coord).size() > 2) {
                    graph.vertices.insert(coord);
                }
            }
        }
    }

    for (const auto& vertex_start : graph.vertices) {
        graph.edges[vertex_start] = get_edges_bfs(grid, vertex_start, graph.vertices);
    }

    return graph;
}

std::pair<bool, int> get_max_distance_dfs(const Grid& grid, const Graph& graph, const Coord& current, const Coord& end, std::set<Coord>& seen) {
    if (current == end) {
        return {true, 0};
    }

    int max_dist = 0;
    seen.insert(current);

    auto it_edges = graph.edges.find(current);
    if (it_edges != graph.edges.end()) {
        for (const auto& edge : it_edges->second) {
            if (!seen.count(edge.end)) {
                auto [is_valid, dist] = get_max_distance_dfs(grid, graph, edge.end, end, seen);
                if (is_valid) {
                    max_dist = std::max(max_dist, dist + edge.weight);
                }
            }
        }
    }
    
    seen.erase(current);

    if (max_dist == 0 && !(current == end)) {
        return {false, 0};
    }
    return {true, max_dist};
}

int solve(const std::vector<std::string>& input_lines) {
    Grid grid = parse_input(input_lines);

    Coord start = {1, 0};
    Coord end = {grid.width - 2, grid.height - 1};

    Graph graph = get_graph(grid, start, end);

    std::set<Coord> seen;
    auto [is_valid, max_dist] = get_max_distance_dfs(grid, graph, start, end, seen);
    return max_dist;
}

std::vector<std::string> read_file(const std::string& file_name) {
    std::vector<std::string> lines;
    std::string line;
    std::ifstream file(file_name);

    if (file.is_open()) {
        while (std::getline(file, line)) {
            lines.push_back(line);
        }
        file.close();
    } else {
        // Error handling for file open failure
        std::cerr << "Unable to open file: " << file_name << std::endl;
        exit(1); // Exit with an error code
    }
    return lines;
}

int main() {
    initSlopeToDir();

    std::vector<std::string> input_lines = read_file("input.txt");
    std::cout << solve(input_lines) << std::endl;

    return 0;
}
