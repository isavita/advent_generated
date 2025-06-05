
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <cmath>
#include <set>
#include <algorithm>
#include <map>
#include <limits>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

struct Asteroid {
    int x, y;
    double angle;
    double dist;

    Asteroid(int px, int py, double pangle, double pdist) : x(px), y(py), angle(pangle), dist(pdist) {}
};

std::vector<std::vector<bool>> readAsteroids(const std::string& filename) {
    std::vector<std::vector<bool>> asteroids;
    std::ifstream file(filename);
    std::string line;
    while (std::getline(file, line)) {
        std::vector<bool> row;
        for (char c : line) {
            row.push_back(c == '#');
        }
        asteroids.push_back(row);
    }
    return asteroids;
}

int countVisibleAsteroids(const std::vector<std::vector<bool>>& asteroids, int sx, int sy) {
    std::set<double> angles;
    for (int y = 0; y < asteroids.size(); ++y) {
        for (int x = 0; x < asteroids[y].size(); ++x) {
            if (asteroids[y][x] && !(x == sx && y == sy)) {
                double angle = std::atan2(static_cast<double>(y - sy), static_cast<double>(x - sx));
                angles.insert(angle);
            }
        }
    }
    return angles.size();
}

std::pair<std::pair<int, int>, int> findBestAsteroidLocation(const std::vector<std::vector<bool>>& asteroids) {
    std::pair<int, int> bestLocation = {0, 0};
    int maxCount = 0;
    for (int y = 0; y < asteroids.size(); ++y) {
        for (int x = 0; x < asteroids[y].size(); ++x) {
            if (asteroids[y][x]) {
                int count = countVisibleAsteroids(asteroids, x, y);
                if (count > maxCount) {
                    maxCount = count;
                    bestLocation = {x, y};
                }
            }
        }
    }
    return {bestLocation, maxCount};
}

std::vector<Asteroid> vaporizeAsteroids(const std::vector<std::vector<bool>>& asteroids, std::pair<int, int> station) {
    std::map<double, std::vector<Asteroid>> asteroidsByAngle;
    int stationX = station.first;
    int stationY = station.second;

    for (int y = 0; y < asteroids.size(); ++y) {
        for (int x = 0; x < asteroids[y].size(); ++x) {
            if (asteroids[y][x] && !(x == stationX && y == stationY)) {
                double angle = std::atan2(static_cast<double>(y - stationY), static_cast<double>(x - stationX));
                double dist = std::hypot(static_cast<double>(x - stationX), static_cast<double>(y - stationY));

                if (angle < -M_PI / 2.0) {
                    angle += 2 * M_PI;
                }
                asteroidsByAngle[angle].emplace_back(x, y, angle, dist);
            }
        }
    }

    for (auto& pair : asteroidsByAngle) {
        std::sort(pair.second.begin(), pair.second.end(), [](const Asteroid& a, const Asteroid& b) {
            return a.dist < b.dist;
        });
    }

    std::vector<Asteroid> vaporized;
    std::vector<double> sortedAngles;
    for (auto const& pair : asteroidsByAngle) {
        sortedAngles.push_back(pair.first);
    }

    size_t currentAngleIdx = 0;
    while (true) {
        bool vaporizedInRound = false;
        for (size_t k = 0; k < sortedAngles.size(); ++k) {
            double angleToProcess = sortedAngles[currentAngleIdx];
            
            if (!asteroidsByAngle[angleToProcess].empty()) {
                vaporized.push_back(asteroidsByAngle[angleToProcess].front());
                asteroidsByAngle[angleToProcess].erase(asteroidsByAngle[angleToProcess].begin());
                vaporizedInRound = true;

                if (vaporized.size() == 200) {
                    return vaporized;
                }
            }
            currentAngleIdx = (currentAngleIdx + 1) % sortedAngles.size();
        }

        if (!vaporizedInRound) {
            break;
        }
    }
    return vaporized;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::vector<std::vector<bool>> asteroids = readAsteroids("input.txt");
    
    std::pair<std::pair<int, int>, int> stationResult = findBestAsteroidLocation(asteroids);
    std::pair<int, int> station = stationResult.first;

    std::vector<Asteroid> vaporized = vaporizeAsteroids(asteroids, station);

    if (vaporized.size() >= 200) {
        long long result = static_cast<long long>(vaporized[199].x) * 100 + vaporized[199].y;
        std::cout << result << std::endl;
    } else {
        std::cout << "Less than 200 asteroids were vaporized." << std::endl;
    }

    return 0;
}
