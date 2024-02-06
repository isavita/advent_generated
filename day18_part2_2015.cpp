
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

    // Ensure corners are always on
    newGrid[0][0] = true;
    newGrid[0][gridSize - 1] = true;
    newGrid[gridSize - 1][0] = true;
    newGrid[gridSize - 1][gridSize - 1] = true;

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
        for (int x = 0; x < line.size(); x++) {
            grid[x][y] = (line[x] == '#');
        }
        y++;
    }

    // Initialize corners as always on
    grid[0][0] = true;
    grid[0][gridSize - 1] = true;
    grid[gridSize - 1][0] = true;
    grid[gridSize - 1][gridSize - 1] = true;

    for (int i = 0; i < steps; i++) {
        grid = step(grid);
    }

    int onCount = 0;
    for (int x = 0; x < gridSize; x++) {
        for (int y = 0; y < gridSize; y++) {
            if (grid[x][y]) {
                onCount++;
            }
        }
    }

    std::cout << onCount << std::endl;

    return 0;
}
