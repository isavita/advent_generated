
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <utility>

int main() {
    std::map<std::pair<int, int>, int> grid;
    int startX = 0;
    int startY = 0;
    int currentY = 0;

    std::ifstream file("input.txt");
    std::string line;
    int maxWidth = 0;

    while (std::getline(file, line)) {
        maxWidth = line.length();
        for (int x = 0; x < line.length(); ++x) {
            if (line[x] == '#') {
                grid[{x, currentY}] = 2;
            }
        }
        currentY++;
    }
    file.close();

    startX = maxWidth / 2;
    startY = currentY / 2;

    static const int dx[] = {0, 1, 0, -1};
    static const int dy[] = {-1, 0, 1, 0};

    int x = startX;
    int y = startY;
    int dir = 0;
    long long infectedCount = 0;

    for (int i = 0; i < 10000000; ++i) {
        std::pair<int, int> pos = {x, y};
        int& currentState = grid[pos];

        if (currentState == 0) {
            dir = (dir - 1 + 4) % 4;
            currentState = 1;
        } else if (currentState == 1) {
            currentState = 2;
            infectedCount++;
        } else if (currentState == 2) {
            dir = (dir + 1) % 4;
            currentState = 3;
        } else {
            dir = (dir + 2) % 4;
            currentState = 0;
        }

        x += dx[dir];
        y += dy[dir];
    }

    std::cout << infectedCount << std::endl;

    return 0;
}
