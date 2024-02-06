
#include <iostream>
#include <fstream>
#include <vector>

int findSafeStep(std::vector<std::vector<char> >& grid);
bool moveEast(std::vector<std::vector<char> >& grid);
bool moveSouth(std::vector<std::vector<char> >& grid);
void freeEmptyPositions(std::vector<std::vector<char> >& grid, std::vector<std::vector<char> >& oldPostion);

int main() {
    std::ifstream inputFile("input.txt");
    std::vector<std::vector<char> > grid;

    std::string line;
    while (std::getline(inputFile, line)) {
        grid.push_back(std::vector<char>(line.begin(), line.end()));
    }

    std::cout << findSafeStep(grid) << std::endl;

    return 0;
}

int findSafeStep(std::vector<std::vector<char> >& grid) {
    int step = 0;
    while (true) {
        bool eastMoved = moveEast(grid);
        bool southMoved = moveSouth(grid);
        step++;

        if (!eastMoved && !southMoved) {
            break;
        }
    }
    return step;
}

bool moveEast(std::vector<std::vector<char> >& grid) {
    bool moved = false;
    int height = grid.size();
    int width = grid[0].size();

    std::vector<std::vector<char> > oldPositions(height, std::vector<char>(width, ' '));
    for (int y = 0; y < height; y++) {
        for (int x = 0; x < width; x++) {
            if (grid[y][x] == '>') {
                int nextX = (x + 1) % width;
                if (grid[y][nextX] == '.') {
                    oldPositions[y][x] = '.';
                    grid[y][nextX] = '>';
                    x++;
                    moved = true;
                }
            }
        }
    }
    freeEmptyPositions(grid, oldPositions);

    return moved;
}

bool moveSouth(std::vector<std::vector<char> >& grid) {
    bool moved = false;
    int height = grid.size();
    int width = grid[0].size();

    std::vector<std::vector<char> > oldPositions(height, std::vector<char>(width, ' '));
    for (int x = 0; x < width; x++) {
        for (int y = 0; y < height; y++) {
            if (grid[y][x] == 'v') {
                int nextY = (y + 1) % height;
                if (grid[nextY][x] == '.') {
                    oldPositions[y][x] = '.';
                    grid[nextY][x] = 'v';
                    y++;
                    moved = true;
                }
            }
        }
    }
    freeEmptyPositions(grid, oldPositions);

    return moved;
}

void freeEmptyPositions(std::vector<std::vector<char> >& grid, std::vector<std::vector<char> >& oldPostion) {
    for (int y = 0; y < grid.size(); y++) {
        for (int x = 0; x < grid[0].size(); x++) {
            if (oldPostion[y][x] == '.') {
                grid[y][x] = '.';
            }
        }
    }
}
