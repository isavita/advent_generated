#include <fstream>
#include <vector>
#include <iostream>

int main() {
    std::ifstream file("input.txt");
    if (!file) {
        std::cerr << "Error opening file" << std::endl;
        return 1;
    }

    std::vector<std::string> grid;
    std::string line;
    while (std::getline(file, line)) {
        grid.push_back(line);
    }

    int x, y;
    for (x = 0; x < grid[0].size(); x++) {
        if (grid[0][x] == '|') {
            break;
        }
    }
    y = 0;

    int dx = 0, dy = 1;
    int steps = 0;

    while (true) {
        if (x < 0 || x >= grid[0].size() || y < 0 || y >= grid.size()) {
            break;
        }

        char cell = grid[y][x];

        if (cell == ' ') {
            break;
        }

        steps++;

        if (cell == '+') {
            if (dx == 0) {
                if (x > 0 && (grid[y][x-1] == '-' || (grid[y][x-1] >= 'A' && grid[y][x-1] <= 'Z'))) {
                    dx = -1;
                    dy = 0;
                } else {
                    dx = 1;
                    dy = 0;
                }
            } else {
                if (y > 0 && (grid[y-1][x] == '|' || (grid[y-1][x] >= 'A' && grid[y-1][x] <= 'Z'))) {
                    dx = 0;
                    dy = -1;
                } else {
                    dx = 0;
                    dy = 1;
                }
            }
        }

        x += dx;
        y += dy;
    }

    std::cout << steps << std::endl;

    return 0;
}