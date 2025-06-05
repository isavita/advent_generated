
#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <unordered_map>
#include <cmath> // For std::abs

// Coord struct to represent a point (x, y)
struct Coord {
    int x, y;

    // operator== is required for std::unordered_map to compare keys
    bool operator==(const Coord& other) const {
        return x == other.x && y == other.y;
    }
};

// Custom hash for Coord to allow its use as a key in std::unordered_map
// Placed in std namespace as per common practice for custom types used with std hash.
namespace std {
    template <>
    struct hash<Coord> {
        size_t operator()(const Coord& c) const {
            // A common and robust way to combine hashes for two integers,
            // often based on algorithms like boost::hash_combine.
            size_t h1 = hash<int>()(c.x);
            size_t h2 = hash<int>()(c.y);
            return h1 ^ (h2 + 0x9e3779b9 + (h1 << 6) + (h1 >> 2));
        }
    };
}

// Grid struct holds grid dimensions and galaxy positions
struct Grid {
    int width;
    int height;
    std::unordered_map<Coord, char> data; // Stores only non-empty cells (galaxies)
};

// Constant character representing an empty space
const char EMPTY_CHAR = '.';

// build_grid parses input lines to create the initial grid structure and galaxy positions
Grid build_grid(const std::vector<std::string>& input_lines) {
    Grid grid;
    if (input_lines.empty()) {
        grid.width = 0;
        grid.height = 0;
        return grid;
    }

    grid.width = input_lines[0].length();
    grid.height = input_lines.size();

    for (int y = 0; y < grid.height; ++y) {
        for (int x = 0; x < grid.width; ++x) {
            if (input_lines[y][x] != EMPTY_CHAR) {
                grid.data[Coord{x, y}] = input_lines[y][x];
            }
        }
    }
    return grid;
}

// get_empty_rows finds and returns the y-indices of all empty rows
std::vector<int> get_empty_rows(const Grid& grid) {
    std::vector<int> empty_rows;
    for (int y = 0; y < grid.height; ++y) {
        bool is_empty = true;
        for (int x = 0; x < grid.width; ++x) {
            if (grid.data.count(Coord{x, y})) { // Check if cell (x,y) contains a galaxy
                is_empty = false;
                break;
            }
        }
        if (is_empty) {
            empty_rows.push_back(y);
        }
    }
    return empty_rows;
}

// get_empty_cols finds and returns the x-indices of all empty columns
std::vector<int> get_empty_cols(const Grid& grid) {
    std::vector<int> empty_cols;
    for (int x = 0; x < grid.width; ++x) {
        bool is_empty = true;
        for (int y = 0; y < grid.height; ++y) {
            if (grid.data.count(Coord{x, y})) { // Check if cell (x,y) contains a galaxy
                is_empty = false;
                break;
            }
        }
        if (is_empty) {
            empty_cols.push_back(x);
        }
    }
    return empty_cols;
}

// calculate_offsets determines how much each index shifts due to expansion.
// offsets[i] will store the count of empty lines encountered before index i.
std::vector<int> calculate_offsets(const std::vector<int>& empty_indexes, int bound) {
    std::vector<int> offsets(bound, 0);
    for (int idx : empty_indexes) {
        for (int i = idx + 1; i < bound; ++i) {
            offsets[i]++;
        }
    }
    return offsets;
}

// calculate_length computes the Manhattan distance between two coordinates
long long calculate_length(Coord c1, Coord c2) {
    // std::abs is overloaded for int, long, long long.
    // The difference (c2.x - c1.x) will be int, then std::abs returns int,
    // which is implicitly converted to long long for dx. This is safe.
    long long dx = std::abs(c2.x - c1.x);
    long long dy = std::abs(c2.y - c1.y);
    return dx + dy;
}

// solve orchestrates the process:
// 1. Builds the initial grid.
// 2. Finds empty rows and columns.
// 3. Calculates expansion offsets.
// 4. Collects original galaxy coordinates.
// 5. Iterates through all unique pairs of galaxies, calculates their expanded coordinates
//    on the fly, and sums their Manhattan distances.
long long solve(const std::vector<std::string>& input_lines, int expansion_factor) {
    Grid grid = build_grid(input_lines);

    std::vector<int> empty_cols = get_empty_cols(grid);
    std::vector<int> empty_rows = get_empty_rows(grid);

    // Determines how many additional lines are inserted for each empty row/column.
    // E.g., if expansion_factor is 2, one line is added. If 1000000, 999999 lines are added.
    long long num_lines_to_add = expansion_factor - 1;

    std::vector<int> dx_offsets = calculate_offsets(empty_cols, grid.width);
    std::vector<int> dy_offsets = calculate_offsets(empty_rows, grid.height);

    std::vector<Coord> galaxies_original_coords;
    for (const auto& pair : grid.data) {
        galaxies_original_coords.push_back(pair.first);
    }

    long long total_distance = 0;
    // Iterate through all unique pairs of galaxies (i.e., (galaxy1, galaxy2) where galaxy1 != galaxy2,
    // and each pair is counted only once).
    for (size_t i = 0; i < galaxies_original_coords.size(); ++i) {
        for (size_t j = i + 1; j < galaxies_original_coords.size(); ++j) {
            Coord c1_orig = galaxies_original_coords[i];
            Coord c2_orig = galaxies_original_coords[j];

            // Calculate expanded coordinates dynamically.
            // The product `dx_offsets[x] * num_lines_to_add` will result in a long long
            // because `num_lines_to_add` is long long, promoting `dx_offsets[x]`.
            // The final sum `c_orig.x + long_long_product` also results in a long long,
            // which is then cast back to int. Given typical problem constraints (grid size ~140,
            // expansion_factor ~10^6), the maximum coordinate (~140 + 140 * 10^6 = 1.4e8)
            // fits within a 32-bit signed integer.
            Coord c1_expanded = {
                c1_orig.x + static_cast<int>(dx_offsets[c1_orig.x] * num_lines_to_add),
                c1_orig.y + static_cast<int>(dy_offsets[c1_orig.y] * num_lines_to_add)
            };
            Coord c2_expanded = {
                c2_orig.x + static_cast<int>(dx_offsets[c2_orig.x] * num_lines_to_add),
                c2_orig.y + static_cast<int>(dy_offsets[c2_orig.y] * num_lines_to_add)
            };

            total_distance += calculate_length(c1_expanded, c2_expanded);
        }
    }

    return total_distance;
}

// read_file reads all lines from a specified file into a vector of strings
std::vector<std::string> read_file(const std::string& file_name) {
    std::vector<std::string> lines;
    std::ifstream file(file_name);
    if (file.is_open()) {
        std::string line;
        while (std::getline(file, line)) {
            lines.push_back(line);
        }
        file.close();
    } else {
        std::cerr << "Error: Unable to open file " << file_name << std::endl;
        exit(1); // Terminate if the input file cannot be opened
    }
    return lines;
}

int main() {
    // Optimize C++ standard streams for faster input/output operations.
    // This unties cin from cout and disables synchronization with C's stdio.
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::vector<std::string> input_lines = read_file("input.txt");
    long long result = solve(input_lines, 1000000);
    std::cout << result << std::endl;

    return 0;
}
