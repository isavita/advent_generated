#include <iostream>
#include <fstream>
#include <vector>
#include <unordered_map>
#include <cmath>

int countVisibleAsteroids(std::vector<std::vector<bool> >& asteroids, int x, int y);

int findBestAsteroidLocation(std::vector<std::vector<bool> >& asteroids) {
    int maxCount = 0;
    for (int y = 0; y < asteroids.size(); y++) {
        for (int x = 0; x < asteroids[y].size(); x++) {
            if (asteroids[y][x]) {
                int count = countVisibleAsteroids(asteroids, x, y);
                if (count > maxCount) {
                    maxCount = count;
                }
            }
        }
    }
    return maxCount;
}

std::vector<std::vector<bool> > readAsteroids(std::string filename) {
    std::ifstream file(filename);
    std::vector<std::vector<bool> > asteroids;
    if (file.is_open()) {
        std::string line;
        while (std::getline(file, line)) {
            std::vector<bool> asteroidRow(line.size());
            for (int i = 0; i < line.size(); i++) {
                asteroidRow[i] = line[i] == '#';
            }
            asteroids.push_back(asteroidRow);
        }
    } else {
        std::cerr << "Error opening file" << std::endl;
    }
    file.close();
    return asteroids;
}

int countVisibleAsteroids(std::vector<std::vector<bool> >& asteroids, int x, int y) {
    std::unordered_map<double, bool> angles;
    for (int otherY = 0; otherY < asteroids.size(); otherY++) {
        for (int otherX = 0; otherX < asteroids[otherY].size(); otherX++) {
            if (asteroids[otherY][otherX] && !(otherX == x && otherY == y)) {
                double angle = std::atan2(otherY - y, otherX - x);
                angles[angle] = true;
            }
        }
    }
    return angles.size();
}

int main() {
    std::vector<std::vector<bool> > asteroids = readAsteroids("input.txt");
    int maxCount = findBestAsteroidLocation(asteroids);
    std::cout << maxCount << std::endl;
    return 0;
}