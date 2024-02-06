
#include <iostream>
#include <fstream>
#include <vector>

bool isLowPoint(std::vector<std::vector<int> >& heightmap, int x, int y);

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening file" << std::endl;
        return 1;
    }

    std::vector<std::vector<int> > heightmap;
    std::string line;
    while (std::getline(file, line)) {
        std::vector<int> row(line.size());
        for (size_t i = 0; i < line.size(); i++) {
            int height = line[i] - '0';
            row[i] = height;
        }
        heightmap.push_back(row);
    }

    int totalRiskLevel = 0;
    for (size_t y = 0; y < heightmap.size(); y++) {
        for (size_t x = 0; x < heightmap[y].size(); x++) {
            if (isLowPoint(heightmap, x, y)) {
                totalRiskLevel += 1 + heightmap[y][x];
            }
        }
    }

    std::cout << totalRiskLevel << std::endl;

    return 0;
}

bool isLowPoint(std::vector<std::vector<int> >& heightmap, int x, int y) {
    int height = heightmap[y][x];
    if (x > 0 && heightmap[y][x - 1] <= height) {
        return false;
    }
    if (x < heightmap[y].size() - 1 && heightmap[y][x + 1] <= height) {
        return false;
    }
    if (y > 0 && heightmap[y - 1][x] <= height) {
        return false;
    }
    if (y < heightmap.size() - 1 && heightmap[y + 1][x] <= height) {
        return false;
    }
    return true;
}
