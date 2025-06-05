
#include <fstream>
#include <iostream>
#include <map>
#include <set>
#include <string>
#include <utility>
#include <vector>

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::ifstream inputFile("input.txt");
    if (!inputFile.is_open()) {
        return 1;
    }

    std::vector<std::string> grid;
    std::string line;
    while (std::getline(inputFile, line)) {
        grid.push_back(line);
    }
    inputFile.close();

    if (grid.empty()) {
        std::cout << 0 << std::endl;
        return 0;
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

    std::set<std::pair<int, int>> antinodes;

    for (const auto& pair : antennas) {
        const std::vector<std::pair<int, int>>& coords = pair.second;
        for (size_t i = 0; i < coords.size(); ++i) {
            for (size_t j = i + 1; j < coords.size(); ++j) {
                const std::pair<int, int>& A = coords[i];
                const std::pair<int, int>& B = coords[j];

                int p1_y = 2 * A.first - B.first;
                int p1_x = 2 * A.second - B.second;

                int p2_y = 2 * B.first - A.first;
                int p2_x = 2 * B.second - A.second;

                if (p1_y >= 0 && p1_y < h && p1_x >= 0 && p1_x < w) {
                    antinodes.insert({p1_y, p1_x});
                }
                if (p2_y >= 0 && p2_y < h && p2_x >= 0 && p2_x < w) {
                    antinodes.insert({p2_y, p2_x});
                }
            }
        }
    }

    std::cout << antinodes.size() << std::endl;

    return 0;
}
