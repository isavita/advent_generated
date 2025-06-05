
#include <fstream>
#include <iostream>
#include <string>
#include <vector>
#include <algorithm> // For std::max
#include <utility>   // For std::pair, std::tie

// Global variables to store map data and dimensions
std::vector<std::string> grid;
std::vector<std::pair<int, int>> rowBoundaries; // min_x, max_x (0-indexed column indices) for each row
std::vector<std::pair<int, int>> colBoundaries; // min_y, max_y (0-indexed row indices) for each column
int numRows;
int numCols;

// Direction vectors (0: Right, 1: Down, 2: Left, 3: Up)
const int DX[] = {1, 0, -1, 0};
const int DY[] = {0, 1, 0, -1};

void readInput(const std::string& filename, std::vector<std::string>& mapLines, std::string& pathStr) {
    std::ifstream file(filename);
    std::string line;
    bool mapSection = true;
    while (std::getline(file, line)) {
        if (line.empty()) {
            mapSection = false;
            continue;
        }
        if (mapSection) {
            mapLines.push_back(line);
        } else {
            pathStr = line;
        }
    }
}

void parseMap(const std::vector<std::string>& mapLines) {
    numRows = mapLines.size();
    numCols = 0;
    for (const auto& line : mapLines) {
        numCols = std::max(numCols, (int)line.length());
    }

    grid.resize(numRows);
    for (int y = 0; y < numRows; ++y) {
        grid[y] = mapLines[y];
        grid[y].resize(numCols, ' '); // Pad with spaces
    }

    rowBoundaries.resize(numRows);
    for (int y = 0; y < numRows; ++y) {
        int min_x = -1, max_x = -1;
        for (int x = 0; x < numCols; ++x) {
            if (grid[y][x] != ' ') {
                if (min_x == -1) {
                    min_x = x;
                }
                max_x = x;
            }
        }
        rowBoundaries[y] = {min_x, max_x};
    }

    colBoundaries.resize(numCols);
    for (int x = 0; x < numCols; ++x) {
        int min_y = -1, max_y = -1;
        for (int y = 0; y < numRows; ++y) {
            if (grid[y][x] != ' ') {
                if (min_y == -1) {
                    min_y = y;
                }
                max_y = y;
            }
        }
        colBoundaries[x] = {min_y, max_y};
    }
}

std::vector<std::pair<int, char>> parsePath(const std::string& pathStr) {
    std::vector<std::pair<int, char>> instructions;
    size_t i = 0;
    while (i < pathStr.length()) {
        if (std::isdigit(pathStr[i])) {
            int steps = 0;
            while (i < pathStr.length() && std::isdigit(pathStr[i])) {
                steps = steps * 10 + (pathStr[i] - '0');
                i++;
            }
            instructions.push_back({steps, 'M'}); // 'M' for Move
        } else {
            instructions.push_back({0, pathStr[i]}); // 'L' or 'R' for Turn
            i++;
        }
    }
    return instructions;
}

std::pair<int, int> findStartingPosition() {
    for (int x = 0; x < numCols; ++x) {
        if (grid[0][x] == '.') {
            return {x, 0}; // Returns 0-indexed (x, y)
        }
    }
    return {-1, -1}; // Should not happen
}

int turnDirection(int currentFacing, char turn) {
    if (turn == 'R') {
        return (currentFacing + 1) % 4;
    } else if (turn == 'L') {
        return (currentFacing - 1 + 4) % 4; // Add 4 to ensure positive result
    }
    return currentFacing; // Invalid turn
}

std::pair<int, int> move(int x, int y, int direction) {
    return {x + DX[direction], y + DY[direction]};
}

long long simulate(const std::vector<std::pair<int, char>>& instructions) {
    int current_x, current_y; // 0-indexed position
    std::tie(current_x, current_y) = findStartingPosition();
    int facing = 0; // 0: Right, 1: Down, 2: Left, 3: Up

    for (const auto& instr : instructions) {
        if (instr.second == 'M') { // Move instruction
            int steps = instr.first;
            for (int s = 0; s < steps; ++s) {
                int next_x, next_y;
                std::tie(next_x, next_y) = move(current_x, current_y, facing);

                // Handle wrapping based on 0-indexed coordinates
                if (facing == 0) { // Right
                    if (next_x > rowBoundaries[current_y].second) {
                        next_x = rowBoundaries[current_y].first;
                    }
                } else if (facing == 2) { // Left
                    if (next_x < rowBoundaries[current_y].first) {
                        next_x = rowBoundaries[current_y].second;
                    }
                } else if (facing == 1) { // Down
                    if (next_y > colBoundaries[current_x].second) {
                        next_y = colBoundaries[current_x].first;
                    }
                } else if (facing == 3) { // Up
                    if (next_y < colBoundaries[current_x].first) {
                        next_y = colBoundaries[current_x].second;
                    }
                }
                
                char tile = grid[next_y][next_x];
                if (tile == '#') {
                    break; // Wall encountered, stop moving for this instruction
                } else if (tile == '.') {
                    current_x = next_x;
                    current_y = next_y;
                }
                // If tile is ' ' (empty space), something went wrong with wrapping logic.
                // The problem implies we always wrap to a '.' or '#'.
            }
        } else { // Turn instruction ('L' or 'R')
            facing = turnDirection(facing, instr.second);
        }
    }

    // Compute password. Convert 0-indexed (current_x, current_y) to 1-indexed (x, y).
    // Formula: 1000 * y + 4 * x + facing
    return 1000LL * (current_y + 1) + 4LL * (current_x + 1) + facing;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::vector<std::string> mapLines;
    std::string pathStr;
    readInput("input.txt", mapLines, pathStr);

    parseMap(mapLines);
    std::vector<std::pair<int, char>> instructions = parsePath(pathStr);
    
    long long password = simulate(instructions);
    std::cout << password << std::endl;

    return 0;
}
