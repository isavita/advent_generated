
#include <iostream>
#include <fstream>
#include <vector>

int main() {
    std::ifstream file("input.txt");
    int serial;
    file >> serial;

    const int gridSize = 300;
    std::vector<std::vector<int> > grid(gridSize, std::vector<int>(gridSize));

    for (int y = 0; y < gridSize; y++) {
        for (int x = 0; x < gridSize; x++) {
            int rackID = x + 11;
            int powerLevel = rackID * (y + 1);
            powerLevel += serial;
            powerLevel *= rackID;
            powerLevel = (powerLevel / 100) % 10;
            powerLevel -= 5;
            grid[y][x] = powerLevel;
        }
    }

    int maxPower = -1 << 31;
    int maxX, maxY;
    for (int y = 0; y < gridSize - 2; y++) {
        for (int x = 0; x < gridSize - 2; x++) {
            int totalPower = 0;
            for (int dy = 0; dy < 3; dy++) {
                for (int dx = 0; dx < 3; dx++) {
                    totalPower += grid[y + dy][x + dx];
                }
            }
            if (totalPower > maxPower) {
                maxPower = totalPower;
                maxX = x + 1;
                maxY = y + 1;
            }
        }
    }

    std::cout << maxX << "," << maxY << std::endl;

    return 0;
}
