
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <sstream>

struct RebootStep {
    bool action;
    int xStart, xEnd, yStart, yEnd, zStart, zEnd;
};

RebootStep parseRebootStep(const std::string& line) {
    RebootStep step;
    std::stringstream ss(line);
    std::string actionStr, rangesStr, temp;

    ss >> actionStr >> rangesStr;
    step.action = (actionStr == "on");

    size_t xPos = rangesStr.find("x=");
    size_t yPos = rangesStr.find("y=");
    size_t zPos = rangesStr.find("z=");
    
    std::string xRange = rangesStr.substr(xPos + 2, yPos - xPos - 3);
    std::string yRange = rangesStr.substr(yPos + 2, zPos - yPos - 3);
    std::string zRange = rangesStr.substr(zPos + 2);

    size_t xSep = xRange.find("..");
    step.xStart = std::stoi(xRange.substr(0, xSep));
    step.xEnd = std::stoi(xRange.substr(xSep + 2));

    size_t ySep = yRange.find("..");
    step.yStart = std::stoi(yRange.substr(0, ySep));
    step.yEnd = std::stoi(yRange.substr(ySep + 2));

    size_t zSep = zRange.find("..");
    step.zStart = std::stoi(zRange.substr(0, zSep));
    step.zEnd = std::stoi(zRange.substr(zSep + 2));

    return step;
}

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening file" << std::endl;
        return 1;
    }

    std::vector<RebootStep> rebootSteps;
    std::string line;
    while (std::getline(file, line)) {
        if (line.empty()) continue;
        rebootSteps.push_back(parseRebootStep(line));
    }
    file.close();

    const int minCoord = -50;
    const int maxCoord = 50;
    const int gridSize = maxCoord - minCoord + 1;

    bool cubeGrid[gridSize][gridSize][gridSize] = {false};

    for (const auto& step : rebootSteps) {
        if (step.xStart < -50 || step.xEnd > 50 || step.yStart < -50 || step.yEnd > 50 || step.zStart < -50 || step.zEnd > 50) {
            continue;
        }

        for (int x = std::max(step.xStart, minCoord); x <= std::min(step.xEnd, maxCoord); ++x) {
            for (int y = std::max(step.yStart, minCoord); y <= std::min(step.yEnd, maxCoord); ++y) {
                for (int z = std::max(step.zStart, minCoord); z <= std::min(step.zEnd, maxCoord); ++z) {
                    cubeGrid[x + 50][y + 50][z + 50] = step.action;
                }
            }
        }
    }

    long long onCubes = 0;
    for (int i = 0; i < gridSize; ++i) {
        for (int j = 0; j < gridSize; ++j) {
            for (int k = 0; k < gridSize; ++k) {
                if (cubeGrid[i][j][k]) {
                    onCubes++;
                }
            }
        }
    }

    std::cout << onCubes << std::endl;

    return 0;
}
