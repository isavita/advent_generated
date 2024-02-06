
#include <iostream>
#include <fstream>
#include <vector>
#include <numeric>

const char Open = '.';
const char Trees = '|';
const char Lumberyard = '#';
const int Size = 50;

std::vector<std::vector<char>> readInput(std::string filename);
std::vector<std::vector<char>> transform(std::vector<std::vector<char>> grid);
char nextAcreState(std::vector<std::vector<char>> grid, int i, int j);
int countAdjacent(std::vector<std::vector<char>> grid, int i, int j, char acreType);
std::pair<int, int> countResources(std::vector<std::vector<char>> grid);

int main() {
    std::vector<std::vector<char>> grid = readInput("input.txt");

    for (int minute = 0; minute < 10; minute++) {
        grid = transform(grid);
    }

    std::pair<int, int> resources = countResources(grid);
    std::cout << resources.first * resources.second << std::endl;

    return 0;
}

std::vector<std::vector<char>> readInput(std::string filename) {
    std::ifstream file(filename);
    std::vector<std::vector<char>> grid;

    if (file.is_open()) {
        std::string line;
        while (std::getline(file, line)) {
            std::vector<char> row(line.begin(), line.end());
            grid.push_back(row);
        }
        file.close();
    }

    return grid;
}

std::vector<std::vector<char>> transform(std::vector<std::vector<char>> grid) {
    std::vector<std::vector<char>> newGrid(grid.size(), std::vector<char>(grid[0].size()));

    for (int i = 0; i < grid.size(); i++) {
        for (int j = 0; j < grid[i].size(); j++) {
            newGrid[i][j] = nextAcreState(grid, i, j);
        }
    }

    return newGrid;
}

char nextAcreState(std::vector<std::vector<char>> grid, int i, int j) {
    switch (grid[i][j]) {
    case Open:
        if (countAdjacent(grid, i, j, Trees) >= 3) {
            return Trees;
        }
        break;
    case Trees:
        if (countAdjacent(grid, i, j, Lumberyard) >= 3) {
            return Lumberyard;
        }
        break;
    case Lumberyard:
        if (countAdjacent(grid, i, j, Lumberyard) >= 1 && countAdjacent(grid, i, j, Trees) >= 1) {
            return Lumberyard;
        }
        return Open;
    }

    return grid[i][j];
}

int countAdjacent(std::vector<std::vector<char>> grid, int i, int j, char acreType) {
    int count = 0;
    for (int x = -1; x <= 1; x++) {
        for (int y = -1; y <= 1; y++) {
            if (x == 0 && y == 0) {
                continue;
            }
            if (i + x >= 0 && i + x < grid.size() && j + y >= 0 && j + y < grid[i].size() && grid[i + x][j + y] == acreType) {
                count++;
            }
        }
    }
    return count;
}

std::pair<int, int> countResources(std::vector<std::vector<char>> grid) {
    int wooded = 0;
    int lumberyards = 0;

    for (int i = 0; i < grid.size(); i++) {
        for (int j = 0; j < grid[i].size(); j++) {
            switch (grid[i][j]) {
            case Trees:
                wooded++;
                break;
            case Lumberyard:
                lumberyards++;
                break;
            }
        }
    }

    return std::make_pair(wooded, lumberyards);
}
