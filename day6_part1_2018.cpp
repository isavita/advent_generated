
#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>

struct Point {
    int X, Y;
};

int abs(int x) {
    if (x < 0) {
        return -x;
    }
    return x;
}

int main() {
    std::ifstream file("input.txt");
    std::vector<Point> points;
    int maxX = 0, maxY = 0;
    std::string line;
    while (std::getline(file, line)) {
        std::vector<std::string> coords;
        size_t pos = 0;
        std::string token;
        while ((pos = line.find(", ")) != std::string::npos) {
            token = line.substr(0, pos);
            coords.push_back(token);
            line.erase(0, pos + 2);
        }
        coords.push_back(line);
        int x = std::stoi(coords[0]);
        int y = std::stoi(coords[1]);
        if (x > maxX) {
            maxX = x;
        }
        if (y > maxY) {
            maxY = y;
        }
        points.push_back({x, y});
    }

    std::vector<std::vector<int> > grid(maxX + 2, std::vector<int>(maxY + 2));
    std::vector<int> areas(points.size());
    std::vector<bool> infinite(points.size());

    for (int i = 0; i < grid.size(); i++) {
        for (int j = 0; j < grid[i].size(); j++) {
            int minDist = maxX + maxY;
            for (int k = 0; k < points.size(); k++) {
                int dist = abs(points[k].X - i) + abs(points[k].Y - j);
                if (dist < minDist) {
                    minDist = dist;
                    grid[i][j] = k;
                } else if (dist == minDist) {
                    grid[i][j] = -1;
                }
            }
            if (grid[i][j] != -1) {
                if (i == 0 || j == 0 || i == maxX + 1 || j == maxY + 1) {
                    infinite[grid[i][j]] = true;
                }
                areas[grid[i][j]]++;
            }
        }
    }

    int maxArea = 0;
    for (int i = 0; i < areas.size(); i++) {
        if (!infinite[i] && areas[i] > maxArea) {
            maxArea = areas[i];
        }
    }

    std::cout << maxArea << std::endl;

    return 0;
}
