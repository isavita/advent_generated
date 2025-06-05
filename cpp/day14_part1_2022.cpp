
#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <sstream>
#include <map>
#include <algorithm>

struct Point {
    int x, y;

    Point(int x_val = 0, int y_val = 0) : x(x_val), y(y_val) {}

    Point add(const Point& other) const {
        return Point(x + other.x, y + other.y);
    }

    bool operator<(const Point& other) const {
        if (y != other.y) {
            return y < other.y;
        }
        return x < other.x;
    }
};

std::pair<Point, Point> bounds(const std::map<Point, bool>& grid) {
    int min_x = 2000000000, min_y = 2000000000;
    int max_x = -2000000000, max_y = -2000000000;

    for (const auto& pair : grid) {
        const Point& p = pair.first;
        min_x = std::min(min_x, p.x);
        min_y = std::min(min_y, p.y);
        max_x = std::max(max_x, p.x);
        max_y = std::max(max_y, p.y);
    }
    return {Point(min_x, min_y), Point(max_x, max_y)};
}

int fill(std::map<Point, bool>& grid) {
    Point min_coords, max_coords;
    std::tie(min_coords, max_coords) = bounds(grid);
    
    int floor = max_coords.y + 1;

    int sands = 0;
    int first_floor_touch = 0;

    std::vector<Point> directions = {Point(0, 1), Point(-1, 1), Point(1, 1)};

    while (true) {
        Point sand = Point(500, 0);

        while (true) {
            if (sand.y == floor - 1) {
                if (first_floor_touch == 0) {
                    first_floor_touch = sands;
                }
                grid[sand] = true;
                break;
            }

            bool moved = false;
            for (const auto& d : directions) {
                Point new_sand = sand.add(d);
                if (grid.find(new_sand) == grid.end()) {
                    sand = new_sand;
                    moved = true;
                    break;
                }
            }

            if (!moved) {
                grid[sand] = true;
                break;
            }
        }
        
        sands++;

        if (sand.y == 0) {
            return first_floor_touch;
        }
    }
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::map<Point, bool> grid;

    std::ifstream inputFile("input.txt");
    std::string line;

    while (std::getline(inputFile, line)) {
        size_t pos = 0;
        while ((pos = line.find(" -> ", pos)) != std::string::npos) {
            line.replace(pos, 4, " ");
            pos += 1; 
        }

        std::vector<Point> points;
        std::istringstream ss(line);
        int x_val, y_val;
        char comma;
        while (ss >> x_val >> comma >> y_val) {
            points.emplace_back(x_val, y_val);
        }

        for (size_t i = 0; i < points.size() - 1; ++i) {
            Point p1 = points[i];
            Point p2 = points[i+1];

            if (p1.x == p2.x) {
                for (int y_fill = std::min(p1.y, p2.y); y_fill <= std::max(p1.y, p2.y); ++y_fill) {
                    grid[Point(p1.x, y_fill)] = true;
                }
            } else {
                for (int x_fill = std::min(p1.x, p2.x); x_fill <= std::max(p1.x, p2.x); ++x_fill) {
                    grid[Point(x_fill, p1.y)] = true;
                }
            }
        }
    }

    std::cout << fill(grid) << std::endl;

    return 0;
}
