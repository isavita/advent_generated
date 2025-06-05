
#include <fstream>
#include <iostream>
#include <map>
#include <string>
#include <utility>

int main() {
    std::ifstream file("input.txt");
    std::map<std::pair<int, int>, bool> grid;

    int startX = 0, startY = 0;
    int currentY = 0;
    std::string line;

    while (std::getline(file, line)) {
        if (line.empty()) continue;

        int currentX = 0;
        for (char c : line) {
            if (c == '#') {
                grid[{currentX, currentY}] = true;
            }
            currentX++;
        }
        if (currentY == 0) {
            startX = (currentX - 1) / 2;
        }
        currentY++;
    }
    startY = (currentY - 1) / 2;

    const int dx[] = {0, 1, 0, -1};
    const int dy[] = {-1, 0, 1, 0};

    int x = startX;
    int y = startY;
    int dir = 0;
    int infectedCount = 0;

    for (int i = 0; i < 10000; ++i) {
        std::pair<int, int> pos = {x, y};

        auto it = grid.find(pos);
        if (it != grid.end()) {
            dir = (dir + 1) % 4;
            grid.erase(it);
        } else {
            dir = (dir - 1 + 4) % 4;
            grid[pos] = true;
            infectedCount++;
        }

        x += dx[dir];
        y += dy[dir];
    }

    std::cout << infectedCount << std::endl;

    return 0;
}

