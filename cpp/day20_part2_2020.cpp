
#include <iostream>
#include <vector>
#include <string>
#include <cmath>
#include <fstream>
#include <sstream>
#include <algorithm> // For std::reverse
#include <unordered_set>

// Helper function to split a string by a delimiter
std::vector<std::string> split(const std::string& s, const std::string& delimiter) {
    std::vector<std::string> tokens;
    std::string::size_type prev_pos = 0;
    std::string::size_type pos = 0;
    while ((pos = s.find(delimiter, prev_pos)) != std::string::npos) {
        tokens.push_back(s.substr(prev_pos, pos - prev_pos));
        prev_pos = pos + delimiter.length();
    }
    tokens.push_back(s.substr(prev_pos)); // Add the last token
    return tokens;
}

// Rotates a square grid 90 degrees clockwise
std::vector<std::string> rotateStringGrid(const std::vector<std::string>& grid) {
    int N = grid.size();
    std::vector<std::string> rotated_grid(N, std::string(N, ' '));
    for (int r = 0; r < N; ++r) {
        for (int c = 0; c < N; ++c) {
            rotated_grid[c][N - 1 - r] = grid[r][c];
        }
    }
    return rotated_grid;
}

// Mirrors a grid horizontally
std::vector<std::string> mirrorStringGrid(const std::vector<std::string>& grid) {
    std::vector<std::string> mirrored_grid = grid;
    for (auto& row : mirrored_grid) {
        std::reverse(row.begin(), row.end());
    }
    return mirrored_grid;
}

// Represents the contents and pre-calculated edges for a specific orientation of a tile
struct OrientedTileData {
    std::vector<std::string> contents;
    std::string top_edge;
    std::string bottom_edge;
    std::string left_edge;
    std::string right_edge;
};

// Represents an original tile, storing its ID and all 8 pre-calculated orientations
struct Tile {
    int id;
    std::vector<OrientedTileData> orientations;
};

// Stores the chosen original tile index and orientation index for a cell in the assembled grid
struct AssembledCellInfo {
    int original_tile_idx;
    int orientation_idx;
};

// Generates all 8 orientations (4 rotations, each mirrored) for an initial grid,
// and pre-calculates their edge strings.
std::vector<OrientedTileData> generateOrientationsAndEdges(const std::vector<std::string>& initial_grid) {
    std::vector<OrientedTileData> all_orientations_data;
    std::vector<std::string> current_grid = initial_grid;

    // Lambda functions to get column/row strings from a grid
    auto get_col = [](const std::vector<std::string>& g, bool first) {
        std::string s;
        for (const auto& row : g) {
            s += (first ? row[0] : row.back());
        }
        return s;
    };
    auto get_row = [](const std::vector<std::string>& g, bool first) {
        return first ? g[0] : g.back();
    };

    for (int i = 0; i < 4; ++i) { // Generate 4 rotations
        // Current rotation
        OrientedTileData data_rot;
        data_rot.contents = current_grid;
        data_rot.top_edge = get_row(current_grid, true);
        data_rot.bottom_edge = get_row(current_grid, false);
        data_rot.left_edge = get_col(current_grid, true);
        data_rot.right_edge = get_col(current_grid, false);
        all_orientations_data.push_back(data_rot);

        // Mirror of current rotation
        std::vector<std::string> mirrored_grid = mirrorStringGrid(current_grid);
        OrientedTileData data_mirror;
        data_mirror.contents = mirrored_grid;
        data_mirror.top_edge = get_row(mirrored_grid, true);
        data_mirror.bottom_edge = get_row(mirrored_grid, false);
        data_mirror.left_edge = get_col(mirrored_grid, true);
        data_mirror.right_edge = get_col(mirrored_grid, false);
        all_orientations_data.push_back(data_mirror);

        current_grid = rotateStringGrid(current_grid); // Rotate for the next iteration
    }
    return all_orientations_data;
}

// Parses tiles from the input string, pre-generating all orientations and their edges
std::vector<Tile> parseTilesFromInput(const std::string& input_str) {
    std::vector<Tile> tiles;
    std::vector<std::string> blocks = split(input_str, "\n\n");
    for (const auto& block : blocks) {
        std::vector<std::string> lines = split(block, "\n");
        std::string id_line = lines[0];
        int tile_id = std::stoi(id_line.substr(5, id_line.length() - 6)); // e.g., "Tile 1234:"

        std::vector<std::string> contents;
        for (size_t i = 1; i < lines.size(); ++i) {
            contents.push_back(lines[i]);
        }
        tiles.push_back({tile_id, generateOrientationsAndEdges(contents)});
    }
    return tiles;
}

// Backtracking function to assemble the puzzle
bool backtrackAssemble(
    const std::vector<Tile>& original_tiles, // All original tiles with pre-computed orientations
    std::vector<std::vector<AssembledCellInfo>>& assembled_grid, // Grid storing chosen tile/orientation indices
    std::unordered_set<int>& used_tile_indices, // Set of indices of original_tiles already used
    int row,
    int col,
    int edge_size
) {
    if (row == edge_size) { // All cells filled, assembly complete
        return true;
    }

    // Determine the next cell to fill
    int next_row = row;
    int next_col = col + 1;
    if (next_col == edge_size) {
        next_row++;
        next_col = 0;
    }

    for (size_t i = 0; i < original_tiles.size(); ++i) { // Iterate through all original tiles
        if (used_tile_indices.count(i)) { // Skip if this tile is already used
            continue;
        }

        const Tile& current_original_tile = original_tiles[i];

        for (size_t orientation_idx = 0; orientation_idx < current_original_tile.orientations.size(); ++orientation_idx) {
            const auto& opt_data = current_original_tile.orientations[orientation_idx];

            // Check if current tile (in this orientation) fits with neighbors
            if (row != 0) { // Check top edge against bottom of tile above
                const auto& above_tile_info = assembled_grid[row - 1][col];
                const auto& above_tile_data = original_tiles[above_tile_info.original_tile_idx].orientations[above_tile_info.orientation_idx];
                if (opt_data.top_edge != above_tile_data.bottom_edge) {
                    continue;
                }
            }
            if (col != 0) { // Check left edge against right of tile to the left
                const auto& left_tile_info = assembled_grid[row][col - 1];
                const auto& left_tile_data = original_tiles[left_tile_info.original_tile_idx].orientations[left_tile_info.orientation_idx];
                if (opt_data.left_edge != left_tile_data.right_edge) {
                    continue;
                }
            }

            // Place tile: record its original index and chosen orientation index
            assembled_grid[row][col] = {static_cast<int>(i), static_cast<int>(orientation_idx)};
            used_tile_indices.insert(i);

            // Recurse to fill the next cell
            if (backtrackAssemble(original_tiles, assembled_grid, used_tile_indices, next_row, next_col, edge_size)) {
                return true; // Solution found
            }

            // Backtrack: current placement didn't lead to a solution, remove tile from used set
            used_tile_indices.erase(i);
        }
    }
    return false; // No tile fits this spot, backtrack
}

// Finds coordinates of all sea monster '#' characters in an image
std::vector<std::pair<int, int>> findMonsterCoords(const std::vector<std::string>& image) {
    std::vector<std::string> monster = {
        "                  # ",
        "#    ##    ##    ###",
        " #  #  #  #  #  #   ",
    };
    std::vector<std::pair<int, int>> monster_offsets; // Relative coordinates of '#' in monster
    for (int r = 0; r < monster.size(); ++r) {
        for (int c = 0; c < monster[r].length(); ++c) {
            if (monster[r][c] == '#') {
                monster_offsets.push_back({r, c});
            }
        }
    }
    int monster_height = monster.size();
    int monster_length = monster[0].length();

    std::vector<std::pair<int, int>> monster_starting_coords; // Top-left corner of found monsters
    for (int r = 0; r <= (int)image.size() - monster_height; ++r) {
        for (int c = 0; c <= (int)image[0].length() - monster_length; ++c) {
            bool monster_found = true;
            for (const auto& offset : monster_offsets) {
                if (image[r + offset.first][c + offset.second] != '#') {
                    monster_found = false;
                    break;
                }
            }
            if (monster_found) {
                monster_starting_coords.push_back({r, c});
            }
        }
    }

    std::vector<std::pair<int, int>> found_monster_coords; // Absolute coordinates of monster's '#'
    for (const auto& start_coord : monster_starting_coords) {
        for (const auto& offset : monster_offsets) {
            found_monster_coords.push_back({start_coord.first + offset.first, start_coord.second + offset.second});
        }
    }
    return found_monster_coords;
}


long long solve(const std::string& input_str) {
    std::vector<Tile> tiles = parseTilesFromInput(input_str);
    int edge_size = static_cast<int>(std::sqrt(tiles.size()));

    std::vector<std::vector<AssembledCellInfo>> assembled_grid(edge_size, std::vector<AssembledCellInfo>(edge_size));
    std::unordered_set<int> used_tile_indices;

    // Assemble the puzzle using backtracking
    backtrackAssemble(tiles, assembled_grid, used_tile_indices, 0, 0, edge_size);

    // Determine the size of a single borderless tile (e.g., 8x8 if original was 10x10)
    // Access the first tile's first orientation to get its content size, then remove borders
    int sub_tile_dim = tiles[assembled_grid[0][0].original_tile_idx]
                             .orientations[assembled_grid[0][0].orientation_idx]
                             .contents.size() - 2;

    int full_image_dim = edge_size * sub_tile_dim;
    std::vector<std::string> final_image(full_image_dim);

    // Construct the full image from the borderless contents of assembled tiles
    for (int big_row = 0; big_row < edge_size; ++big_row) {
        for (int sub_row = 0; sub_row < sub_tile_dim; ++sub_row) {
            std::string current_image_row;
            for (int big_col = 0; big_col < edge_size; ++big_col) {
                const auto& cell_info = assembled_grid[big_row][big_col];
                const auto& oriented_tile_data = tiles[cell_info.original_tile_idx].orientations[cell_info.orientation_idx];
                // Append the borderless part of the current sub-tile's row
                current_image_row += oriented_tile_data.contents[sub_row + 1].substr(1, sub_tile_dim);
            }
            final_image[big_row * sub_tile_dim + sub_row] = current_image_row;
        }
    }

    // Generate all 8 orientations of the final assembled image to find monsters
    std::vector<std::vector<std::string>> image_orientations_for_monster_search;
    std::vector<std::string> current_image_orientation = final_image;
    for(int i = 0; i < 4; ++i) { // 4 rotations
        image_orientations_for_monster_search.push_back(current_image_orientation);
        image_orientations_for_monster_search.push_back(mirrorStringGrid(current_image_orientation));
        current_image_orientation = rotateStringGrid(current_image_orientation); // Rotate for next iteration
    }

    std::vector<std::pair<int, int>> monster_coords;
    for (const auto& opt_image : image_orientations_for_monster_search) {
        monster_coords = findMonsterCoords(opt_image);
        if (!monster_coords.empty()) {
            final_image = opt_image; // Found the correct orientation of the image
            break;
        }
    }

    // Mark sea monster locations with 'O'
    for (const auto& coord : monster_coords) {
        final_image[coord.first][coord.second] = 'O';
    }

    // Count the number of '#' characters remaining (rough waters)
    long long rough_waters_count = 0;
    for (const auto& row_str : final_image) {
        for (char c : row_str) {
            if (c == '#') {
                rough_waters_count++;
            }
        }
    }
    return rough_waters_count;
}

int main() {
    std::ifstream ifs("input.txt");
    if (!ifs.is_open()) {
        std::cerr << "Error: Could not open input.txt" << std::endl;
        return 1;
    }
    std::stringstream buffer;
    buffer << ifs.rdbuf();
    std::string input_str = buffer.str();
    ifs.close();

    // Remove any trailing newlines or carriage returns to match Python's .strip() behavior
    while (!input_str.empty() && (input_str.back() == '\n' || input_str.back() == '\r')) {
        input_str.pop_back();
    }

    std::cout << solve(input_str) << std::endl;

    return 0;
}
