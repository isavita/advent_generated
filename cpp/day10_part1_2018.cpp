
#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <regex>
#include <algorithm>
#include <limits>
#include <utility>

struct Vec2 {
    int x, y;
};

struct LightPoint {
    Vec2 pos;
    Vec2 vel;
};

std::vector<LightPoint> readInput(const std::string& filePath) {
    std::vector<LightPoint> points;
    std::ifstream file(filePath);
    std::string line;
    std::regex pattern("-?\\d+");

    while (std::getline(file, line)) {
        std::sregex_iterator it(line.begin(), line.end(), pattern);
        std::sregex_iterator end;
        int values[4];
        int i = 0;
        for (; it != end && i < 4; ++it, ++i) {
            values[i] = std::stoi(it->str());
        }
        points.push_back({{values[0], values[1]}, {values[2], values[3]}});
    }
    return points;
}

std::pair<int, std::vector<Vec2>> simulate(std::vector<LightPoint> points) {
    long long prevArea = std::numeric_limits<long long>::max();
    std::vector<Vec2> bestPoints;
    int bestSecond = 0;

    int seconds = 0;
    while (true) {
        int minX = std::numeric_limits<int>::max();
        int maxX = std::numeric_limits<int>::min();
        int minY = std::numeric_limits<int>::max();
        int maxY = std::numeric_limits<int>::min();

        for (const auto& p : points) {
            minX = std::min(minX, p.pos.x);
            maxX = std::max(maxX, p.pos.x);
            minY = std::min(minY, p.pos.y);
            maxY = std::max(maxY, p.pos.y);
        }

        long long currentArea = (long long)(maxX - minX) * (maxY - minY);

        if (currentArea > prevArea) {
            return {bestSecond, bestPoints};
        }

        prevArea = currentArea;
        bestSecond = seconds;
        bestPoints.clear();
        bestPoints.reserve(points.size());
        for (const auto& p : points) {
            bestPoints.push_back(p.pos);
        }

        for (auto& p : points) {
            p.pos.x += p.vel.x;
            p.pos.y += p.vel.y;
        }

        seconds++;
    }
}

void printMessage(const std::vector<Vec2>& points, int second) {
    int minX = std::numeric_limits<int>::max();
    int maxX = std::numeric_limits<int>::min();
    int minY = std::numeric_limits<int>::max();
    int maxY = std::numeric_limits<int>::min();

    for (const auto& p : points) {
        minX = std::min(minX, p.x);
        maxX = std::max(maxX, p.x);
        minY = std::min(minY, p.y);
        maxY = std::max(maxY, p.y);
    }

    if (points.empty()) {
        return;
    }

    int width = maxX - minX + 1;
    int height = maxY - minY + 1;

    std::vector<std::string> sky(height, std::string(width, '.'));

    for (const auto& p : points) {
        sky[p.y - minY][p.x - minX] = '#';
    }

    std::cout << "After " << second << " seconds:\n";
    for (const auto& row : sky) {
        std::cout << row << '\n';
    }
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::vector<LightPoint> initialPoints = readInput("input.txt");
    std::pair<int, std::vector<Vec2>> result = simulate(initialPoints);
    printMessage(result.second, result.first);

    return 0;
}
