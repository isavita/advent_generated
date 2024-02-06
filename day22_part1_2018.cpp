
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>

std::pair<int, std::pair<int, int> > parseInput(std::string data);
std::vector<std::vector<int> > makeCaveSystem(int depth, std::pair<int, int> target);
int calculateRiskLevel(std::vector<std::vector<int> > cave, std::pair<int, int> target);

int main() {
    std::ifstream inputFile("input.txt");
    std::stringstream buffer;
    buffer << inputFile.rdbuf();
    std::string data = buffer.str();

    std::pair<int, std::pair<int, int> > parsedInput = parseInput(data);
    int depth = parsedInput.first;
    std::pair<int, int> target = parsedInput.second;

    std::vector<std::vector<int> > cave = makeCaveSystem(depth, target);
    int riskLevel = calculateRiskLevel(cave, target);
    std::cout << "Total Risk Level: " << riskLevel << std::endl;

    return 0;
}

std::pair<int, std::pair<int, int> > parseInput(std::string data) {
    std::istringstream ss(data);
    std::string line;
    std::getline(ss, line);
    int depth = std::stoi(line.substr(line.find(" ")+1));
    std::getline(ss, line);
    line = line.substr(line.find(" ")+1);
    int x = std::stoi(line.substr(0, line.find(",")));
    int y = std::stoi(line.substr(line.find(",")+1));
    return std::make_pair(depth, std::make_pair(x, y));
}

std::vector<std::vector<int> > makeCaveSystem(int depth, std::pair<int, int> target) {
    std::vector<std::vector<int> > cave(target.second + 1, std::vector<int>(target.first + 1));
    for (int y = 0; y <= target.second; y++) {
        for (int x = 0; x <= target.first; x++) {
            int geologicIndex;
            if ((x == 0 && y == 0) || (x == target.first && y == target.second)) {
                geologicIndex = 0;
            } else if (y == 0) {
                geologicIndex = x * 16807;
            } else if (x == 0) {
                geologicIndex = y * 48271;
            } else {
                geologicIndex = cave[y][x-1] * cave[y-1][x];
            }
            cave[y][x] = (geologicIndex + depth) % 20183;
        }
    }
    return cave;
}

int calculateRiskLevel(std::vector<std::vector<int> > cave, std::pair<int, int> target) {
    int riskLevel = 0;
    for (int y = 0; y <= target.second; y++) {
        for (int x = 0; x <= target.first; x++) {
            riskLevel += cave[y][x] % 3;
        }
    }
    return riskLevel;
}
