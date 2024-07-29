#include <iostream>
#include <fstream>
#include <vector>
#include <set>
#include <string>

const int SIZE = 5;
const char BUG = '#';
const char EMPTY = '.';

std::vector<std::string> readInput(const std::string& filename) {
    std::ifstream file(filename);
    std::vector<std::string> grid(SIZE);
    for (int i = 0; i < SIZE; ++i) {
        std::getline(file, grid[i]);
    }
    return grid;
}

int countAdjacentBugs(const std::vector<std::string>& grid, int x, int y) {
    int count = 0;
    for (int dx = -1; dx <= 1; ++dx) {
        for (int dy = -1; dy <= 1; ++dy) {
            if (abs(dx) + abs(dy) == 1) {
                int nx = x + dx, ny = y + dy;
                if (nx >= 0 && nx < SIZE && ny >= 0 && ny < SIZE && grid[nx][ny] == BUG) {
                    count++;
                }
            }
        }
    }
    return count;
}

std::string gridToString(const std::vector<std::string>& grid) {
    std::string result;
    for (const auto& row : grid) {
        result += row;
    }
    return result;
}

int calculateBiodiversity(const std::string& layout) {
    int rating = 0;
    for (int i = 0; i < layout.size(); ++i) {
        if (layout[i] == BUG) {
            rating += (1 << i);
        }
    }
    return rating;
}

int main() {
    std::vector<std::string> grid = readInput("input.txt");
    std::set<std::string> seenLayouts;
    
    while (true) {
        std::string currentLayout = gridToString(grid);
        if (seenLayouts.count(currentLayout)) {
            std::cout << calculateBiodiversity(currentLayout) << std::endl;
            break;
        }
        seenLayouts.insert(currentLayout);
        
        std::vector<std::string> newGrid = grid;
        for (int i = 0; i < SIZE; ++i) {
            for (int j = 0; j < SIZE; ++j) {
                int adjacentBugs = countAdjacentBugs(grid, i, j);
                if (grid[i][j] == BUG) {
                    newGrid[i][j] = (adjacentBugs == 1) ? BUG : EMPTY;
                } else {
                    newGrid[i][j] = (adjacentBugs == 1 || adjacentBugs == 2) ? BUG : EMPTY;
                }
            }
        }
        grid = newGrid;
    }
    return 0;
}