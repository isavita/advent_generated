
#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <set>
#include <fstream>
#include <algorithm> // For std::find

struct Coord {
    int x;
    int y;

    Coord(int x_val = 0, int y_val = 0) : x(x_val), y(y_val) {}

    bool operator==(const Coord& other) const {
        return x == other.x && y == other.y;
    }

    bool operator!=(const Coord& other) const {
        return !(*this == other);
    }

    bool operator<(const Coord& other) const {
        if (y != other.y) {
            return y < other.y;
        }
        return x < other.x;
    }

    Coord add(const Coord& other) const {
        return Coord(x + other.x, y + other.y);
    }

    Coord subtract(const Coord& other) const {
        return Coord(x - other.x, y - other.y);
    }

    Coord opposite() const {
        return Coord(-x, -y);
    }
};

const Coord Undefined(0, 0);
const Coord Top(0, -1);
const Coord Right(1, 0);
const Coord Bottom(0, 1);
const Coord Left(-1, 0);

const char Empty = '.';
const char Start = 'S';
const char Vertical = '|';
const char Horizontal = '-';
const char TopLeftCorner = 'J';
const char TopRightCorner = 'L';
const char BottomLeftCorner = '7';
const char BottomRightCorner = 'F';

using Pipe = std::set<Coord>;

const Pipe VerticalPipe = {Top, Bottom};
const Pipe HorizontalPipe = {Left, Right};
const Pipe TopLeftCornerPipe = {Top, Left};
const Pipe TopRightCornerPipe = {Top, Right};
const Pipe BottomLeftCornerPipe = {Bottom, Left};
const Pipe BottomRightCornerPipe = {Bottom, Right};

std::map<char, Pipe> TileToPipe;

void initializeTileToPipe() {
    TileToPipe[Vertical] = VerticalPipe;
    TileToPipe[Horizontal] = HorizontalPipe;
    TileToPipe[TopLeftCorner] = TopLeftCornerPipe;
    TileToPipe[TopRightCorner] = TopRightCornerPipe;
    TileToPipe[BottomLeftCorner] = BottomLeftCornerPipe;
    TileToPipe[BottomRightCorner] = BottomRightCornerPipe;
}

Pipe get_pipe_from_tile(char tile) {
    auto it = TileToPipe.find(tile);
    if (it != TileToPipe.end()) {
        return it->second;
    }
    return {};
}

char get_tile_from_pipe(const Pipe& pipe) {
    for (const auto& pair : TileToPipe) {
        if (pipe == pair.second) { // std::set operator== compares contents
            return pair.first;
        }
    }
    return Empty;
}

std::map<Coord, char> build_grid(const std::vector<std::string>& input_lines) {
    std::map<Coord, char> grid;
    for (int y = 0; y < input_lines.size(); ++y) {
        for (int x = 0; x < input_lines[y].length(); ++x) {
            char c = input_lines[y][x];
            if (c != Empty) {
                grid[Coord(x, y)] = c;
            }
        }
    }
    return grid;
}

Coord find_start(const std::map<Coord, char>& grid) {
    for (const auto& pair : grid) {
        if (pair.second == Start) {
            return pair.first;
        }
    }
    return Undefined;
}

Pipe get_pipe_from_neighbors(const Coord& coord, const std::map<Coord, char>& grid) {
    Pipe pipe;
    std::vector<Coord> directions = {Top, Right, Bottom, Left};

    for (const auto& dir : directions) {
        Coord neighbor_coord = coord.add(dir);
        auto it = grid.find(neighbor_coord);
        if (it != grid.end()) {
            Pipe neighbor_pipe = get_pipe_from_tile(it->second);
            if (neighbor_pipe.count(dir.opposite())) {
                pipe.insert(dir);
            }
        }
    }
    return pipe;
}

std::vector<Coord> path_finding(const Coord& start, const std::map<Coord, char>& grid) {
    std::vector<Coord> path;
    path.push_back(start);

    Pipe start_pipe = get_pipe_from_neighbors(start, grid);
    Coord previous_dir = Undefined;
    Coord current = Undefined;

    for (const auto& dir : start_pipe) {
        previous_dir = dir;
        current = start.add(dir);
        break;
    }

    while (current != start) {
        path.push_back(current);
        Pipe current_pipe = get_pipe_from_tile(grid.at(current));
        for (const auto& dir : current_pipe) {
            if (dir != previous_dir.opposite()) {
                previous_dir = dir;
                current = current.add(dir);
                break;
            }
        }
    }
    return path;
}

std::map<Coord, char> get_path_grid(const std::map<Coord, char>& grid, const std::vector<Coord>& path, char empty_char) {
    std::map<Coord, char> new_grid;
    for (const auto& coord : path) {
        new_grid[coord] = grid.at(coord);
    }
    Coord start = path[0];
    new_grid[start] = get_tile_from_pipe(get_pipe_from_neighbors(start, grid));
    return new_grid;
}

bool is_inside(const Coord& coord, const std::map<Coord, char>& path_grid, char empty_char) {
    if (path_grid.count(coord)) {
        return false;
    }

    char start_pipe_char = empty_char;
    int num_pipe_on_left = 0;

    for (int x = 0; x < coord.x; ++x) {
        Coord c(x, coord.y);
        auto it = path_grid.find(c);
        if (it != path_grid.end()) {
            char v = it->second;
            if (v == Vertical) {
                num_pipe_on_left++;
            } else if (v == TopRightCorner) { // L
                start_pipe_char = TopRightCorner;
            } else if (v == BottomRightCorner) { // F
                start_pipe_char = BottomRightCorner;
            } else if (v == TopLeftCorner) { // J
                if (start_pipe_char == BottomRightCorner) { // F...J
                    start_pipe_char = empty_char;
                    num_pipe_on_left++;
                } else if (start_pipe_char == TopRightCorner) { // L...J
                    start_pipe_char = empty_char;
                }
            } else if (v == BottomLeftCorner) { // 7
                if (start_pipe_char == TopRightCorner) { // L...7
                    start_pipe_char = empty_char;
                    num_pipe_on_left++;
                } else if (start_pipe_char == BottomRightCorner) { // F...7
                    start_pipe_char = empty_char;
                }
            }
        }
    }
    return num_pipe_on_left % 2 == 1;
}

int solve(const std::vector<std::string>& input_lines) {
    std::map<Coord, char> grid = build_grid(input_lines);
    Coord start = find_start(grid);
    std::vector<Coord> path = path_finding(start, grid);
    std::map<Coord, char> path_grid = get_path_grid(grid, path, Empty);

    int count = 0;
    int max_y = input_lines.size();
    int max_x = input_lines.empty() ? 0 : input_lines[0].length();

    for (int y = 0; y < max_y; ++y) {
        for (int x = 0; x < max_x; ++x) {
            Coord c(x, y);
            if (is_inside(c, path_grid, Empty)) {
                count++;
            }
        }
    }
    return count;
}

std::vector<std::string> read_file(const std::string& file_name) {
    std::vector<std::string> lines;
    std::ifstream file(file_name);
    if (file.is_open()) {
        std::string line;
        while (std::getline(file, line)) {
            lines.push_back(line);
        }
        file.close();
    }
    return lines;
}

int main() {
    initializeTileToPipe();
    std::vector<std::string> input_lines = read_file("input.txt");
    std::cout << solve(input_lines) << std::endl;
    return 0;
}
