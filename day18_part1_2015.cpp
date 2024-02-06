
#include <iostream>
#include <fstream>
#include <vector>

const int gridSize = 100;
const int steps = 100;

int countOnNeighbors(std::vector<std::vector<bool> >& grid, int x, int y) {
    int on = 0;
    for (int dx = -1; dx <= 1; dx++) {
        for (int dy = -1; dy <= 1; dy++) {
            if (dx == 0 && dy == 0) {
                continue;
            }
            int nx = x + dx;
            int ny = y + dy;
            if (nx >= 0 && nx < gridSize && ny >= 0 && ny < gridSize && grid[nx][ny]) {
                on++;
            }
        }
    }
    return on;
}

std::vector<std::vector<bool> > step(std::vector<std::vector<bool> >& grid) {
    std::vector<std::vector<bool> > newGrid(gridSize, std::vector<bool>(gridSize, false));

    for (int x = 0; x < gridSize; x++) {
        for (int y = 0; y < gridSize; y++) {
            int onNeighbors = countOnNeighbors(grid, x, y);
            if (grid[x][y]) {
                newGrid[x][y] = (onNeighbors == 2 || onNeighbors == 3);
            } else {
                newGrid[x][y] = (onNeighbors == 3);
            }
        }
    }

    return newGrid;
}

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cout << "Error opening file" << std::endl;
        return 1;
    }

    std::vector<std::vector<bool> > grid(gridSize, std::vector<bool>(gridSize, false));

    std::string line;
    int y = 0;
    while (std::getline(file, line)) {
        for (int x = 0; x < gridSize; x++) {
            grid[x][y] = (line[x] == '#');
        }
        y++;
    }

    for (int i = 0; i < steps; i++) {
        grid = step(grid);
    }

    int onCount = 0;
    for (const auto& row : grid) {
        for (bool light : row) {
            if (light) {
                onCount++;
            }
        }
    }

    std::cout << onCount << std::endl;

    return 0;
}
