
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>

struct Coord {
    float x;
    float y;
    float z;
};

struct Point {
    Coord pos;
    Coord vel;
};

std::vector<Point> parseInput(std::vector<std::string> input) {
    std::vector<Point> points(input.size());
    for (int i = 0; i < input.size(); i++) {
        Point point;
        sscanf(input[i].c_str(), "%f, %f, %f @ %f, %f, %f",
               &point.pos.x, &point.pos.y, &point.pos.z,
               &point.vel.x, &point.vel.y, &point.vel.z);
        points[i] = point;
    }
    return points;
}

bool isIntersecting2D(Point p1, Point p2, Coord& coord, float& time1, float& time2) {
    float det = p1.vel.x * p2.vel.y - p2.vel.x * p1.vel.y;
    if (det == 0) {
        return false;
    }
    time1 = (p2.vel.y * (p2.pos.x - p1.pos.x) - p2.vel.x * (p2.pos.y - p1.pos.y)) / det;
    time2 = (p1.vel.y * (p2.pos.x - p1.pos.x) - p1.vel.x * (p2.pos.y - p1.pos.y)) / det;
    coord.x = p1.pos.x + p1.vel.x * time1;
    coord.y = p1.pos.y + p1.vel.y * time1;
    coord.z = 0;
    return true;
}

int solve(std::vector<std::string> input, float min, float max) {
    std::vector<Point> points = parseInput(input);

    int cnt = 0;
    for (int i = 0; i < points.size(); i++) {
        for (int j = 0; j < i; j++) {
            Coord coord;
            float time1, time2;
            if (isIntersecting2D(points[i], points[j], coord, time1, time2)) {
                bool isInBound = min <= coord.x && coord.x <= max && min <= coord.y && coord.y <= max;
                if (isInBound && time1 >= 0 && time2 >= 0) {
                    cnt++;
                }
            }
        }
    }
    return cnt;
}

std::vector<std::string> readFile(std::string fileName) {
    std::ifstream file(fileName);
    if (!file.is_open()) {
        throw std::runtime_error("Error opening file");
    }

    std::vector<std::string> lines;
    std::string line;
    while (std::getline(file, line)) {
        lines.push_back(line);
    }

    return lines;
}

int main() {
    std::vector<std::string> input = readFile("input.txt");
    std::cout << solve(input, 200000000000000, 400000000000000) << std::endl;
    return 0;
}
