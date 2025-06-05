
#include <iostream>
#include <vector>
#include <string>
#include <queue>
#include <unordered_map>
#include <unordered_set>
#include <fstream>
#include <stdexcept>

struct Coord {
    int x;
    int y;

    bool operator==(const Coord& other) const {
        return x == other.x && y == other.y;
    }

    Coord operator+(const Coord& other) const {
        return {x + other.x, y + other.y};
    }
};

// Custom hash for Coord to use in unordered_map/set
namespace std {
    template <>
    struct hash<Coord> {
        size_t operator()(const Coord& c) const {
            // A common way to combine two hashes
            size_t h1 = hash<int>()(c.x);
            size_t h2 = hash<int>()(c.y);
            return h1 ^ (h2 << 1); // Or boost::hash_combine equivalent
        }
    };
}

struct Grid {
    int width;
    int height;
    std::unordered_map<Coord, char> data; // Stores only non-'.' chars

    bool is_in_bounds(const Coord& coord) const {
        return coord.x >= 0 && coord.x < width && coord.y >= 0 && coord.y < height;
    }
};

Grid parse_input(const std::vector<std::string>& input_lines) {
    if (input_lines.empty()) {
        return {0, 0, {}};
    }
    int height = input_lines.size();
    int width = input_lines[0].length();
    std::unordered_map<Coord, char> data;

    for (int y = 0; y < height; ++y) {
        for (int x = 0; x < width; ++x) {
            char ch = input_lines[y][x];
            if (ch != '.') {
                data[{x, y}] = ch;
            }
        }
    }
    return {width, height, data};
}

Coord find_start(const Grid& grid) {
    for (const auto& pair : grid.data) {
        if (pair.second == 'S') {
            return pair.first;
        }
    }
    throw std::runtime_error("No start 'S' found.");
}

std::vector<Coord> neighbors4(const Grid& grid, const Coord& coord) {
    static const std::vector<Coord> directions = {{0, -1}, {0, 1}, {1, 0}, {-1, 0}};
    std::vector<Coord> valid_neighbors;
    for (const auto& dir : directions) {
        Coord neighbor = coord + dir;
        if (grid.is_in_bounds(neighbor)) {
            // Check if the neighbor is not a '#'
            auto it = grid.data.find(neighbor);
            if (it == grid.data.end() || it->second != '#') { // If not found, it's '.' (passable)
                valid_neighbors.push_back(neighbor);
            }
        }
    }
    return valid_neighbors;
}

std::unordered_map<Coord, int> breadth_first_search(const Grid& grid, const Coord& start) {
    std::queue<Coord> frontier;
    std::unordered_set<Coord> reached;
    std::unordered_map<Coord, int> distances;

    frontier.push(start);
    reached.insert(start);
    distances[start] = 0;

    while (!frontier.empty()) {
        Coord current = frontier.front();
        frontier.pop();

        for (const auto& next : neighbors4(grid, current)) {
            if (reached.find(next) == reached.end()) { // If not reached
                frontier.push(next);
                reached.insert(next);
                distances[next] = distances[current] + 1;
            }
        }
    }
    return distances;
}

int solve(const std::vector<std::string>& input_lines, int num_steps) {
    Grid grid = parse_input(input_lines);
    Coord start = find_start(grid);
    std::unordered_map<Coord, int> distances = breadth_first_search(grid, start);

    int count = 0;
    for (const auto& pair : distances) {
        int dist = pair.second;
        if (dist <= num_steps && dist % 2 == 0) {
            count++;
        }
    }
    return count;
}

std::vector<std::string> read_file(const std::string& file_name) {
    std::vector<std::string> lines;
    std::ifstream file(file_name);
    if (!file.is_open()) {
        throw std::runtime_error("Could not open file: " + file_name);
    }
    std::string line;
    while (std::getline(file, line)) {
        lines.push_back(line);
    }
    file.close();
    return lines;
}

int main() {
    try {
        std::vector<std::string> input_lines = read_file("input.txt");
        std::cout << solve(input_lines, 64) << std::endl;
    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << std::endl;
        return 1;
    }
    return 0;
}
