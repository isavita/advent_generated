
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <algorithm>
#include <sstream>

const int MAX_COORD = 999;

int main() {
    std::vector<std::vector<int>> grid(MAX_COORD + 1, std::vector<int>(MAX_COORD + 1, 0));
    int overlap_count = 0;

    std::ifstream file("input.txt");

    std::string line;
    while (std::getline(file, line)) {
        int x1, y1, x2, y2;
        char comma1, comma2;
        std::string arrow;

        std::stringstream ss(line);
        ss >> x1 >> comma1 >> y1 >> arrow >> x2 >> comma2 >> y2;

        if (ss.fail() || arrow != "->") continue;

        if (x1 == x2) {
            for (int y = std::min(y1, y2); y <= std::max(y1, y2); ++y) {
                if (x1 >= 0 && x1 <= MAX_COORD && y >= 0 && y <= MAX_COORD) {
                    grid[x1][y]++;
                    if (grid[x1][y] == 2) {
                        overlap_count++;
                    }
                }
            }
        } else if (y1 == y2) {
            for (int x = std::min(x1, x2); x <= std::max(x1, x2); ++x) {
                if (x >= 0 && x <= MAX_COORD && y1 >= 0 && y1 <= MAX_COORD) {
                    grid[x][y1]++;
                    if (grid[x][y1] == 2) {
                        overlap_count++;
                    }
                }
            }
        }
    }

    std::cout << overlap_count << std::endl;

    return 0;
}
