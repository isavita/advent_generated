
#include <iostream>
#include <vector>
#include <string>
#include <sstream>
#include <fstream>
#include <cmath>

const int MAX_COORD = 1000;

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::vector<std::vector<int>> grid(MAX_COORD, std::vector<int>(MAX_COORD, 0));

    std::ifstream inputFile("input.txt");
    std::string line;
    while (std::getline(inputFile, line)) {
        int x1, y1, x2, y2;
        char comma1, arrow1, arrow2, comma2;
        std::stringstream ss(line);
        ss >> x1 >> comma1 >> y1 >> arrow1 >> arrow2 >> x2 >> comma2 >> y2;

        int xStep = 0;
        if (x2 > x1) xStep = 1;
        else if (x2 < x1) xStep = -1;

        int yStep = 0;
        if (y2 > y1) yStep = 1;
        else if (y2 < y1) yStep = -1;

        int currentX = x1;
        int currentY = y1;

        while (true) {
            grid[currentX][currentY]++;
            if (currentX == x2 && currentY == y2) {
                break;
            }
            currentX += xStep;
            currentY += yStep;
        }
    }
    inputFile.close();

    int count = 0;
    for (int i = 0; i < MAX_COORD; ++i) {
        for (int j = 0; j < MAX_COORD; ++j) {
            if (grid[i][j] > 1) {
                count++;
            }
        }
    }

    std::cout << count << std::endl;

    return 0;
}
