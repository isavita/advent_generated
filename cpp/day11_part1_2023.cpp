
#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <utility> // For std::pair
#include <cmath>   // For std::abs
#include <fstream> // For file operations
#include <numeric> // Not used, but often useful

// Using Point as an alias for std::pair<int, int> for clarity
using Point = std::pair<int, int>;

// Structure to represent the grid
struct Grid {
    int width;
    int height;
    std::map<Point, char> data; // Stores non-empty characters at their coordinates
};

// Builds the grid from input lines
Grid build_grid(const std::vector<std::string>& input_lines, char empty_char = '.') {
    Grid grid;
    grid.height = input_lines.size();
    grid.width = (grid.height > 0) ? input_lines[0].length() : 0;

    for (int y = 0; y < grid.height; ++y) {
        for (int x = 0; x < grid.width; ++x) {
            char current_char = input_lines[y][x];
            if (current_char != empty_char) {
                grid.data[{x, y}] = current_char;
            }
        }
    }
    return grid;
}

// Finds indices of empty rows
std::vector<int> get_empty_rows(const Grid& grid) {
    std::vector<int> empty_rows;
    for (int y = 0; y < grid.height; ++y) {
        bool is_row_empty = true;
        for (int x = 0; x < grid.width; ++x) {
            if (grid.data.count({x, y})) { // Check if a character exists at (x, y)
                is_row_empty = false;
                break;
            }
        }
        if (is_row_empty) {
            empty_rows.push_back(y);
        }
    }
    return empty_rows;
}

// Finds indices of empty columns
std::vector<int> get_empty_cols(const Grid& grid) {
    std::vector<int> empty_cols;
    for (int x = 0; x < grid.width; ++x) {
        bool is_col_empty = true;
        for (int y = 0; y < grid.height; ++y) {
            if (grid.data.count({x, y})) { // Check if a character exists at (x, y)
                is_col_empty = false;
                break;
            }
        }
        if (is_col_empty) {
            empty_cols.push_back(x);
        }
    }
    return empty_cols;
}

// Calculates expansion offsets for coordinates
// offsets[i] stores the count of empty_indexes that are strictly less than i.
std::vector<int> calculate_offsets(const std::vector<int>& empty_indexes, int bound) {
    std::vector<int> offsets(bound, 0);
    int current_offset = 0;
    size_t empty_idx_ptr = 0; // Pointer for empty_indexes (assumed sorted)

    for (int i = 0; i < bound; ++i) {
        offsets[i] = current_offset;
        // If the current index 'i' is an empty index, increment the offset for subsequent indices
        if (empty_idx_ptr < empty_indexes.size() && empty_indexes[empty_idx_ptr] == i) {
            current_offset++;
            empty_idx_ptr++;
        }
    }
    return offsets;
}

// Expands the grid based on empty rows and columns
Grid expand_grid(const Grid& grid, int expansion_factor) {
    std::vector<int> empty_cols = get_empty_cols(grid);
    std::vector<int> empty_rows = get_empty_rows(grid);
    int lines_to_add_per_empty_space = expansion_factor - 1;

    Grid new_grid;
    new_grid.width = grid.width + empty_cols.size() * lines_to_add_per_empty_space;
    new_grid.height = grid.height + empty_rows.size() * lines_to_add_per_empty_space;

    std::vector<int> dx_offsets = calculate_offsets(empty_cols, grid.width);
    std::vector<int> dy_offsets = calculate_offsets(empty_rows, grid.height);

    // Relocate galaxies in the new grid
    for (const auto& entry : grid.data) {
        Point old_coord = entry.first;
        char value = entry.second;

        int old_x = old_coord.first;
        int old_y = old_coord.second;

        int new_x = old_x + dx_offsets[old_x] * lines_to_add_per_empty_space;
        int new_y = old_y + dy_offsets[old_y] * lines_to_add_per_empty_space;

        new_grid.data[{new_x, new_y}] = value;
    }
    return new_grid;
}

// Calculates Manhattan distance between two points
long long calculate_length(Point c1, Point c2) {
    long long dx = std::abs(static_cast<long long>(c2.first) - c1.first);
    long long dy = std::abs(static_cast<long long>(c2.second) - c1.second);
    return dx + dy;
}

// Solves the main problem
long long solve(const std::vector<std::string>& input_lines) {
    Grid grid = build_grid(input_lines);

    // Expansion factor for Part 1 is 2
    Grid expanded_grid = expand_grid(grid, 2);

    // Extract galaxy coordinates into a vector for easier iteration over pairs
    std::vector<Point> galaxy_coords;
    for (const auto& entry : expanded_grid.data) {
        galaxy_coords.push_back(entry.first);
    }

    long long total_length = 0;
    // Calculate Manhattan distance between all unique pairs of galaxies
    for (size_t i = 0; i < galaxy_coords.size(); ++i) {
        for (size_t j = i + 1; j < galaxy_coords.size(); ++j) {
            total_length += calculate_length(galaxy_coords[i], galaxy_coords[j]);
        }
    }

    return total_length;
}

// Reads all lines from a file
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
        std::cerr << "Error: Unable to open file: " << file_name << std::endl;
    }
    return lines;
}

int main() {
    // Optimize C++ standard streams for faster I/O (optional, but good practice)
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::vector<std::string> input_lines = read_file("input.txt");
    std::cout << solve(input_lines) << std::endl;

    return 0;
}
