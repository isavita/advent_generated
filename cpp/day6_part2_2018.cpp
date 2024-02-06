
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <cmath>

struct coordinate {
    int x, y;
};

std::vector<coordinate> parseCoordinates(std::string input);
int findRegionSize(std::vector<coordinate> coordinates, int maxDistance);
std::tuple<int, int, int, int> findBoundingBox(std::vector<coordinate> coordinates);
int manhattanDistance(int x1, int y1, int x2, int y2);

int main() {
    std::ifstream file("input.txt");
    std::stringstream buffer;
    buffer << file.rdbuf();
    std::string content = buffer.str();

    std::vector<coordinate> coordinates = parseCoordinates(content);

    int regionSize = findRegionSize(coordinates, 10000);

    std::cout << regionSize << std::endl;

    return 0;
}

std::vector<coordinate> parseCoordinates(std::string input) {
    std::vector<coordinate> coordinates;
    std::stringstream ss(input);
    std::string line;

    while (std::getline(ss, line, '\n')) {
        coordinate c;
        std::sscanf(line.c_str(), "%d, %d", &c.x, &c.y);
        coordinates.push_back(c);
    }

    return coordinates;
}

int findRegionSize(std::vector<coordinate> coordinates, int maxDistance) {
    auto [minX, minY, maxX, maxY] = findBoundingBox(coordinates);
    int regionSize = 0;

    for (int x = minX; x <= maxX; x++) {
        for (int y = minY; y <= maxY; y++) {
            int totalDistance = 0;

            for (const auto& c : coordinates) {
                totalDistance += manhattanDistance(x, y, c.x, c.y);
            }

            if (totalDistance < maxDistance) {
                regionSize++;
            }
        }
    }

    return regionSize;
}

std::tuple<int, int, int, int> findBoundingBox(std::vector<coordinate> coordinates) {
    int minX = std::numeric_limits<int>::max();
    int minY = std::numeric_limits<int>::max();
    int maxX = std::numeric_limits<int>::min();
    int maxY = std::numeric_limits<int>::min();

    for (const auto& c : coordinates) {
        if (c.x < minX) {
            minX = c.x;
        }
        if (c.y < minY) {
            minY = c.y;
        }
        if (c.x > maxX) {
            maxX = c.x;
        }
        if (c.y > maxY) {
            maxY = c.y;
        }
    }

    return std::make_tuple(minX, minY, maxX, maxY);
}

int manhattanDistance(int x1, int y1, int x2, int y2) {
    return std::abs(x1 - x2) + std::abs(y1 - y2);
}
