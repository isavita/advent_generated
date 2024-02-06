#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>

const int gridSize = 1000;

void processInstruction(std::string instruction, std::vector<std::vector<int> >& grid);
int totalBrightness(std::vector<std::vector<int> >& grid);

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cout << "Error opening file" << std::endl;
        return 1;
    }

    std::vector<std::vector<int> > grid(gridSize, std::vector<int>(gridSize, 0));
    std::string line;
    while (std::getline(file, line)) {
        processInstruction(line, grid);
    }
    file.close();

    std::cout << totalBrightness(grid) << std::endl;

    return 0;
}

void processInstruction(std::string instruction, std::vector<std::vector<int> >& grid) {
    std::istringstream iss(instruction);
    std::vector<std::string> parts;
    std::string part;
    while (iss >> part) {
        parts.push_back(part);
    }

    int startX, startY, endX, endY;
    sscanf(parts[parts.size()-3].c_str(), "%d,%d", &startX, &startY);
    sscanf(parts[parts.size()-1].c_str(), "%d,%d", &endX, &endY);

    for (int x = startX; x <= endX; x++) {
        for (int y = startY; y <= endY; y++) {
            if (instruction.find("turn on") != std::string::npos) {
                grid[x][y]++;
            } else if (instruction.find("turn off") != std::string::npos) {
                if (grid[x][y] > 0) {
                    grid[x][y]--;
                }
            } else if (instruction.find("toggle") != std::string::npos) {
                grid[x][y] += 2;
            }
        }
    }
}

int totalBrightness(std::vector<std::vector<int> >& grid) {
    int brightness = 0;
    for (const auto& row : grid) {
        for (int light : row) {
            brightness += light;
        }
    }
    return brightness;
}