#include <iostream>
#include <fstream>
#include <vector>
#include <sstream>
#include <unordered_set>
#include <queue>

struct Point {
    int x, y, z, w;

    Point(int x, int y, int z, int w) : x(x), y(y), z(z), w(w) {}

    int manhattanDistance(const Point& other) const {
        return abs(x - other.x) + abs(y - other.y) + abs(z - other.z) + abs(w - other.w);
    }
};

void bfs(int start, const std::vector<Point>& points, std::vector<bool>& visited) {
    std::queue<int> q;
    q.push(start);
    visited[start] = true;

    while (!q.empty()) {
        int current = q.front();
        q.pop();

        for (int i = 0; i < points.size(); ++i) {
            if (!visited[i] && points[current].manhattanDistance(points[i]) <= 3) {
                visited[i] = true;
                q.push(i);
            }
        }
    }
}

int countConstellations(const std::vector<Point>& points) {
    std::vector<bool> visited(points.size(), false);
    int constellations = 0;

    for (int i = 0; i < points.size(); ++i) {
        if (!visited[i]) {
            bfs(i, points, visited);
            ++constellations;
        }
    }

    return constellations;
}

std::vector<Point> readPointsFromFile(const std::string& filename) {
    std::ifstream file(filename);
    std::vector<Point> points;
    std::string line;

    while (std::getline(file, line)) {
        std::istringstream iss(line);
        int x, y, z, w;
        char comma; // To consume commas
        iss >> x >> comma >> y >> comma >> z >> comma >> w;
        points.emplace_back(x, y, z, w);
    }

    return points;
}

int main() {
    std::vector<Point> points = readPointsFromFile("input.txt");
    int result = countConstellations(points);
    std::cout << result << std::endl;
    return 0;
}