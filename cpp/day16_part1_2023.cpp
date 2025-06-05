
#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <deque>
#include <unordered_set>

struct Coord {
    int x, y;

    bool operator==(const Coord& other) const {
        return x == other.x && y == other.y;
    }

    Coord add(const Coord& other) const {
        return {x + other.x, y + other.y};
    }

    Coord rotate90() const {
        return {y, -x};
    }

    Coord rotate_neg90() const {
        return {-y, x};
    }
};

namespace std {
    template <>
    struct hash<Coord> {
        size_t operator()(const Coord& c) const {
            return std::hash<int>()(c.x) ^ (std::hash<int>()(c.y) << 1);
        }
    };
}

struct Beam {
    Coord origin;
    Coord dir;

    bool operator==(const Beam& other) const {
        return origin == other.origin && dir == other.dir;
    }
};

namespace std {
    template <>
    struct hash<Beam> {
        size_t operator()(const Beam& b) const {
            return std::hash<Coord>()(b.origin) ^ (std::hash<Coord>()(b.dir) << 1);
        }
    };
}

class Grid {
public:
    int width;
    int height;
    std::vector<std::string> data;

    Grid(const std::vector<std::string>& input_lines) {
        height = input_lines.size();
        width = height > 0 ? input_lines[0].length() : 0;
        data = input_lines;
    }

    char get(const Coord& coord) const {
        return data[coord.y][coord.x];
    }

    bool is_in_bounds(const Coord& coord) const {
        return coord.x >= 0 && coord.x < width && coord.y >= 0 && coord.y < height;
    }
};

const Coord NORTH = {0, -1};
const Coord WEST = {-1, 0};
const Coord SOUTH = {0, 1};
const Coord EAST = {1, 0};

bool is_horizontal(const Coord& dir) {
    return dir == EAST || dir == WEST;
}

bool is_vertical(const Coord& dir) {
    return dir == NORTH || dir == SOUTH;
}

std::vector<Beam> next_beam(const Grid& grid, const Beam& beam) {
    std::vector<Beam> beams;
    char cell_char = grid.get(beam.origin);

    if (cell_char == '.') {
        beams.push_back({beam.origin.add(beam.dir), beam.dir});
    } else if (cell_char == '/') {
        Coord new_dir;
        if (is_vertical(beam.dir)) {
            new_dir = beam.dir.rotate_neg90();
        } else {
            new_dir = beam.dir.rotate90();
        }
        beams.push_back({beam.origin.add(new_dir), new_dir});
    } else if (cell_char == '\\') {
        Coord new_dir;
        if (is_vertical(beam.dir)) {
            new_dir = beam.dir.rotate90();
        } else {
            new_dir = beam.dir.rotate_neg90();
        }
        beams.push_back({beam.origin.add(new_dir), new_dir});
    } else if (cell_char == '|') {
        if (is_horizontal(beam.dir)) {
            Coord new_dir1 = beam.dir.rotate90();
            Coord new_dir2 = beam.dir.rotate_neg90();
            beams.push_back({beam.origin.add(new_dir1), new_dir1});
            beams.push_back({beam.origin.add(new_dir2), new_dir2});
        } else {
            beams.push_back({beam.origin.add(beam.dir), beam.dir});
        }
    } else if (cell_char == '-') {
        if (is_vertical(beam.dir)) {
            Coord new_dir1 = beam.dir.rotate90();
            Coord new_dir2 = beam.dir.rotate_neg90();
            beams.push_back({beam.origin.add(new_dir1), new_dir1});
            beams.push_back({beam.origin.add(new_dir2), new_dir2});
        } else {
            beams.push_back({beam.origin.add(beam.dir), beam.dir});
        }
    }
    return beams;
}

std::unordered_set<Beam> calculate_propagation(const Grid& grid, const Beam& start_beam) {
    std::unordered_set<Beam> already_seen;
    std::deque<Beam> to_explore;

    to_explore.push_back(start_beam);

    while (!to_explore.empty()) {
        Beam beam = to_explore.front();
        to_explore.pop_front();

        if (grid.is_in_bounds(beam.origin)) {
            if (already_seen.find(beam) == already_seen.end()) {
                already_seen.insert(beam);
                std::vector<Beam> next_beams = next_beam(grid, beam);
                for (const auto& next_b : next_beams) {
                    to_explore.push_back(next_b);
                }
            }
        }
    }
    return already_seen;
}

int calculate_energization(const std::unordered_set<Beam>& already_seen) {
    std::unordered_set<Coord> already_energized;
    for (const auto& beam : already_seen) {
        already_energized.insert(beam.origin);
    }
    return already_energized.size();
}

int solve(const std::vector<std::string>& input_lines) {
    Grid grid(input_lines);
    Beam start_beam = {{0, 0}, EAST};

    std::unordered_set<Beam> propagated_beams = calculate_propagation(grid, start_beam);
    return calculate_energization(propagated_beams);
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
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::vector<std::string> input_lines = read_file("input.txt");
    if (input_lines.empty()) {
        return 1;
    }

    std::cout << solve(input_lines) << std::endl;

    return 0;
}
