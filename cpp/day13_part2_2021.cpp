
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <set>
#include <utility>
#include <algorithm>

struct Point {
    int x;
    int y;

    bool operator<(const Point& other) const {
        if (y != other.y) {
            return y < other.y;
        }
        return x < other.x;
    }
};

struct Fold {
    char axis;
    int value;
};

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::ifstream file("input.txt");
    std::set<Point> points;
    std::vector<Fold> folds;
    bool readingPoints = true;
    std::string line;

    while (std::getline(file, line)) {
        if (line.empty()) {
            readingPoints = false;
            continue;
        }

        if (readingPoints) {
            size_t commaPos = line.find(',');
            int x = std::stoi(line.substr(0, commaPos));
            int y = std::stoi(line.substr(commaPos + 1));
            points.insert({x, y});
        } else {
            size_t eqPos = line.find('=');
            char axis = line[eqPos - 1];
            int value = std::stoi(line.substr(eqPos + 1));
            folds.push_back({axis, value});
        }
    }
    file.close();

    for (size_t i = 0; i < folds.size(); ++i) {
        const Fold& fold = folds[i];
        std::set<Point> newPoints;

        for (const auto& p : points) {
            if (fold.axis == 'x' && p.x > fold.value) {
                newPoints.insert({2 * fold.value - p.x, p.y});
            } else if (fold.axis == 'y' && p.y > fold.value) {
                newPoints.insert({p.x, 2 * fold.value - p.y});
            } else {
                newPoints.insert(p);
            }
        }
        points = newPoints;

        if (i == 0) {
            std::cout << points.size() << std::endl;
        }
    }

    int maxX = 0;
    int maxY = 0;
    for (const auto& p : points) {
        maxX = std::max(maxX, p.x);
        maxY = std::max(maxY, p.y);
    }

    std::vector<std::string> grid(maxY + 1, std::string(maxX + 1, '.'));
    for (const auto& p : points) {
        grid[p.y][p.x] = '#';
    }

    for (const auto& row : grid) {
        std::cout << row << std::endl;
    }

    return 0;
}

