
#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <numeric> // For std::gcd
#include <map>
#include <set>
#include <tuple>   // For std::tuple
#include <utility> // For std::pair

void solve() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        return;
    }

    std::vector<std::string> grid;
    std::string line;
    while (std::getline(file, line)) {
        grid.push_back(line);
    }
    file.close();

    if (grid.empty()) {
        std::cout << 0 << std::endl;
        return;
    }

    int h = grid.size();
    int w = grid[0].length();

    std::map<char, std::vector<std::pair<int, int>>> antennas;
    for (int y = 0; y < h; ++y) {
        for (int x = 0; x < w; ++x) {
            char c = grid[y][x];
            if (c != '.') {
                antennas[c].push_back({y, x});
            }
        }
    }

    std::map<char, std::set<std::tuple<int, int, int>>> lines_per_freq;
    for (auto const& [freq, coords] : antennas) {
        int n = coords.size();
        for (int i = 0; i < n; ++i) {
            for (int j = i + 1; j < n; ++j) {
                std::pair<int, int> A = coords[i];
                std::pair<int, int> B = coords[j];

                int dy = B.first - A.first;
                int dx = B.second - A.second;

                // gcd(0,0) is implementation defined in C++17, but std::gcd
                // guarantees non-negative.
                // For a pair of distinct points (i+1 ensures this), dy and dx won't both be zero.
                int common_divisor = std::gcd(dy, dx);
                int sy = dy / common_divisor;
                int sx = dx / common_divisor;

                // Normalize (sx, sy) to a canonical form:
                // Make sx positive, or if sx is 0, make sy positive.
                if (sx < 0 || (sx == 0 && sy < 0)) {
                    sx = -sx;
                    sy = -sy;
                }
                
                // Line equation: sy * X - sx * Y = C
                // (X, Y) are the coordinates (column, row)
                // Use point A (A.second, A.first) to find C
                int c = sy * A.second - sx * A.first;
                
                lines_per_freq[freq].insert(std::make_tuple(sx, sy, c));
            }
        }
    }

    std::set<std::pair<int, int>> antinodes;
    for (auto const& [freq, lines] : lines_per_freq) {
        for (auto const& line_tuple : lines) {
            int sx = std::get<0>(line_tuple);
            int sy = std::get<1>(line_tuple);
            int c = std::get<2>(line_tuple);

            if (sy == 0) { // Horizontal line: -sx * Y = C => Y = -C / sx
                // This means dy was 0, so line is horizontal at some Y.
                if (c % sx == 0) {
                    int y_coord = -c / sx;
                    if (y_coord >= 0 && y_coord < h) {
                        for (int x_coord = 0; x_coord < w; ++x_coord) {
                            antinodes.insert({y_coord, x_coord});
                        }
                    }
                }
            } else if (sx == 0) { // Vertical line: sy * X = C => X = C / sy
                // This means dx was 0, so line is vertical at some X.
                if (c % sy == 0) {
                    int x_coord = c / sy;
                    if (x_coord >= 0 && x_coord < w) {
                        for (int y_coord = 0; y_coord < h; ++y_coord) {
                            antinodes.insert({y_coord, x_coord});
                        }
                    }
                }
            } else { // General line: sy * X - sx * Y = c
                // Iterate over all possible Y (rows) and calculate X (column)
                for (int y_coord = 0; y_coord < h; ++y_coord) {
                    // sy * X = c + sx * Y
                    int val = c + sx * y_coord;
                    if (val % sy == 0) {
                        int x_coord = val / sy;
                        if (x_coord >= 0 && x_coord < w) {
                            antinodes.insert({y_coord, x_coord});
                        }
                    }
                }
            }
        }
    }

    std::cout << antinodes.size() << std::endl;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);
    std::cout.tie(NULL);

    solve();

    return 0;
}
