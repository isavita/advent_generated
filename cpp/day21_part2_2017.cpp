
#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <sstream>
#include <unordered_map>
#include <algorithm>

std::vector<std::string> split(const std::string& s, char delimiter) {
    std::vector<std::string> tokens;
    std::string token;
    std::istringstream tokenStream(s);
    while (std::getline(tokenStream, token, delimiter)) {
        tokens.push_back(token);
    }
    return tokens;
}

std::string join(const std::vector<std::string>& parts, char delimiter) {
    std::string s;
    for (size_t i = 0; i < parts.size(); ++i) {
        s += parts[i];
        if (i < parts.size() - 1) {
            s += delimiter;
        }
    }
    return s;
}

std::vector<std::string> rotate_pattern(const std::vector<std::string>& p) {
    int size = p.size();
    std::vector<std::string> rotated_p(size, std::string(size, ' '));
    for (int x = 0; x < size; ++x) {
        for (int y = 0; y < size; ++y) {
            rotated_p[x][y] = p[size - 1 - y][x];
        }
    }
    return rotated_p;
}

std::vector<std::string> flip_pattern(const std::vector<std::string>& p) {
    std::vector<std::string> flipped_p = p;
    for (auto& row : flipped_p) {
        std::reverse(row.begin(), row.end());
    }
    return flipped_p;
}

std::unordered_map<std::string, std::string> rules;

int main() {
    std::ifstream file("input.txt");
    std::string line;

    while (std::getline(file, line)) {
        size_t arrow_pos = line.find(" => ");
        std::string lhs_str = line.substr(0, arrow_pos);
        std::string rhs_str = line.substr(arrow_pos + 4);

        std::vector<std::string> current_lhs_pattern = split(lhs_str, '/');
        
        for (int i = 0; i < 4; ++i) {
            rules[join(current_lhs_pattern, '/')] = rhs_str;
            current_lhs_pattern = rotate_pattern(current_lhs_pattern);
        }

        current_lhs_pattern = flip_pattern(split(lhs_str, '/'));
        for (int i = 0; i < 4; ++i) {
            rules[join(current_lhs_pattern, '/')] = rhs_str;
            current_lhs_pattern = rotate_pattern(current_lhs_pattern);
        }
    }

    std::vector<std::string> grid = {
        ".#.",
        "..#",
        "###",
    };

    for (int iter = 0; iter < 18; ++iter) {
        int current_grid_size = grid.size();
        int sub_size;
        int new_sub_grid_dim;

        if (current_grid_size % 2 == 0) {
            sub_size = 2;
            new_sub_grid_dim = 3;
        } else {
            sub_size = 3;
            new_sub_grid_dim = 4;
        }
        int num_blocks = current_grid_size / sub_size;
        int new_grid_size = num_blocks * new_sub_grid_dim;

        std::vector<std::string> new_grid(new_grid_size, std::string(new_grid_size, ' '));

        for (int block_y = 0; block_y < num_blocks; ++block_y) {
            for (int block_x = 0; block_x < num_blocks; ++block_x) {
                std::vector<std::string> square_parts;
                for (int dy = 0; dy < sub_size; ++dy) {
                    square_parts.push_back(grid[block_y * sub_size + dy].substr(block_x * sub_size, sub_size));
                }
                
                std::string enhanced_square_str = rules.at(join(square_parts, '/'));
                std::vector<std::string> enhanced_square_parts = split(enhanced_square_str, '/');

                for (size_t dy = 0; dy < enhanced_square_parts.size(); ++dy) {
                    new_grid[block_y * new_sub_grid_dim + dy].replace(
                        block_x * new_sub_grid_dim,
                        enhanced_square_parts[dy].length(),
                        enhanced_square_parts[dy]
                    );
                }
            }
        }
        grid = new_grid;
    }

    long long count = 0;
    for (const auto& row : grid) {
        for (char pixel : row) {
            if (pixel == '#') {
                count++;
            }
        }
    }
    std::cout << count << std::endl;

    return 0;
}
