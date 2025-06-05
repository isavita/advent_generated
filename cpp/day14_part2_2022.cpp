
#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <limits>
#include <fstream>
#include <sstream>

struct Coord {
    int c, r;
};

Coord parseCoord(const std::string& s) {
    size_t commaPos = s.find(',');
    return {std::stoi(s.substr(0, commaPos)), std::stoi(s.substr(commaPos + 1))};
}

const int ExtraLeftSpace = 200;

std::vector<std::vector<char>> parseInput(const std::string& input_data, int& origin_col_out) {
    std::vector<std::vector<Coord>> coord_sets;
    int lowest_col = std::numeric_limits<int>::max();
    int highest_row = 0;

    std::stringstream ss(input_data);
    std::string line;
    while (std::getline(ss, line)) {
        std::vector<Coord> coords;
        size_t start = 0;
        size_t arrow_pos;
        while ((arrow_pos = line.find(" -> ", start)) != std::string::npos) {
            Coord c = parseCoord(line.substr(start, arrow_pos - start));
            coords.push_back(c);
            lowest_col = std::min(lowest_col, c.c);
            highest_row = std::max(highest_row, c.r);
            start = arrow_pos + 4;
        }
        Coord c = parseCoord(line.substr(start));
        coords.push_back(c);
        lowest_col = std::min(lowest_col, c.c);
        highest_row = std::max(highest_row, c.r);
        coord_sets.push_back(coords);
    }

    int adjust_col_by = lowest_col - ExtraLeftSpace;
    int max_adjusted_col = 0;

    for (auto& set : coord_sets) {
        for (auto& coord : set) {
            coord.c -= adjust_col_by;
            max_adjusted_col = std::max(max_adjusted_col, coord.c);
        }
    }

    int matrix_height = highest_row + 3;
    int matrix_width = max_adjusted_col + ExtraLeftSpace * 2;

    origin_col_out = 500 - adjust_col_by;

    std::vector<std::vector<char>> matrix(matrix_height, std::vector<char>(matrix_width, '.'));

    for (const auto& set : coord_sets) {
        for (size_t i = 1; i < set.size(); ++i) {
            int c1 = set[i-1].c, r1 = set[i-1].r;
            int c2 = set[i].c, r2 = set[i].r;

            if (c1 == c2) {
                for (int r = std::min(r1, r2); r <= std::max(r1, r2); ++r) {
                    matrix[r][c1] = '#';
                }
            } else if (r1 == r2) {
                for (int c = std::min(c1, c2); c <= std::max(c1, c2); ++c) {
                    matrix[r1][c] = '#';
                }
            }
        }
    }

    matrix[0][origin_col_out] = '+';

    for (int c = 0; c < matrix_width; ++c) {
        matrix[matrix_height - 1][c] = '#';
    }

    return matrix;
}

bool dropSand(std::vector<std::vector<char>>& matrix, int origin_col) {
    int r = 0;
    int c = origin_col;

    while (r < matrix.size() - 1) {
        if (matrix[r+1][c] == '.') {
            r++;
        } else if (c - 1 >= 0 && matrix[r+1][c-1] == '.') {
            r++;
            c--;
        } else if (c + 1 < matrix[0].size() && matrix[r+1][c+1] == '.') {
            r++;
            c++;
        } else {
            matrix[r][c] = 'o';
            return false;
        }
    }
    return true;
}

int solve(const std::string& input_data) {
    int origin_col;
    std::vector<std::vector<char>> matrix = parseInput(input_data, origin_col);

    int ans = 0;
    while (true) {
        if (matrix[0][origin_col] == 'o') {
            break;
        }
        dropSand(matrix, origin_col);
        ans++;
    }
    return ans;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::ifstream inputFile("input.txt");
    std::stringstream buffer;
    buffer << inputFile.rdbuf();
    std::string input_data = buffer.str();
    inputFile.close();

    size_t first = input_data.find_first_not_of(" \t\n\r\f\v");
    if (std::string::npos == first) {
        input_data.clear();
    } else {
        size_t last = input_data.find_last_not_of(" \t\n\r\f\v");
        input_data = input_data.substr(first, (last - first + 1));
    }

    std::cout << solve(input_data) << std::endl;

    return 0;
}
