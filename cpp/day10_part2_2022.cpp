
#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <cmath>
#include <array>
#include <algorithm>

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::vector<int> x;
    x.push_back(1);

    std::ifstream inputFile("input.txt");
    std::string line;

    while (std::getline(inputFile, line)) {
        if (line == "noop") {
            x.push_back(x.back());
        } else {
            int n = std::stoi(line.substr(line.find(' ') + 1));
            x.push_back(x.back());
            x.push_back(x.back() + n);
        }
    }
    inputFile.close();

    std::array<std::array<char, 40>, 6> grid;

    for (int r = 0; r < 6; ++r) {
        for (int c = 0; c < 40; ++c) {
            grid[r][c] = '.';
        }
    }

    int max_cycles_for_screen = 6 * 40;
    for (int i = 0; i < std::min((int)x.size(), max_cycles_for_screen); ++i) {
        int crtx = i % 40;
        int crty = i / 40;
        int val = x[i];

        if (std::abs(crtx - val) <= 1) {
            grid[crty][crtx] = '#';
        }
    }

    for (int r = 0; r < 6; ++r) {
        for (int c = 0; c < 40; ++c) {
            std::cout << grid[r][c];
        }
        std::cout << '\n';
    }

    return 0;
}

