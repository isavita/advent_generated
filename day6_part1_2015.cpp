#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>

const int gridSize = 1000;

void processInstruction(std::string instruction, std::vector<std::vector<bool> >& grid);
int countLights(std::vector<std::vector<bool> >& grid);

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cout << "Error opening file" << std::endl;
        return 1;
    }

    std::vector<std::vector<bool> > grid(gridSize, std::vector<bool>(gridSize, false));

    std::string line;
    while (std::getline(file, line)) {
        processInstruction(line, grid);
    }

    file.close();

    std::cout << countLights(grid) << std::endl;

    return 0;
}

void processInstruction(std::string instruction, std::vector<std::vector<bool> >& grid) {
    std::istringstream iss(instruction);
    std::vector<std::string> parts(std::istream_iterator<std::string>{iss}, std::istream_iterator<std::string>());

    int startX, startY, endX, endY;
    std::sscanf(parts[parts.size()-3].c_str(), "%d,%d", &startX, &startY);
    std::sscanf(parts[parts.size()-1].c_str(), "%d,%d", &endX, &endY);

    for (int x = startX; x <= endX; x++) {
        for (int y = startY; y <= endY; y++) {
            if (instruction.find("turn on") != std::string::npos) {
                grid[x][y] = true;
            } else if (instruction.find("turn off") != std::string::npos) {
                grid[x][y] = false;
            } else if (instruction.find("toggle") != std::string::npos) {
                grid[x][y] = !grid[x][y];
            }
        }
    }
}

int countLights(std::vector<std::vector<bool> >& grid) {
    int count = 0;
    for (const auto& row : grid) {
        for (bool light : row) {
            if (light) {
                count++;
            }
        }
    }
    return count;
}