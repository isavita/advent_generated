
#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <vector>
#include <cmath>
#include <numeric>

struct Coord {
    long long x, y;
};

long long abs_ll(long long x) {
    return std::abs(x);
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::ifstream inputFile("input.txt");
    if (!inputFile.is_open()) {
         // Error handling omitted for brevity as requested
         return 1;
    }

    std::vector<Coord> vertices;
    Coord current = {0, 0};
    vertices.push_back(current);
    long long perimeter = 0;
    std::string line;

    while (std::getline(inputFile, line)) {
        size_t open_paren = line.find('(');
        size_t close_paren = line.find(')');
        std::string color = line.substr(open_paren + 2, close_paren - open_paren - 2);

        std::string length_str = color.substr(0, 5);
        char dir_char = color[5];

        long long length = std::stoll(length_str, nullptr, 16);
        perimeter += length;

        long long dx = 0, dy = 0;
        switch (dir_char) {
            case '0': dx = length; break;  // East
            case '1': dy = length; break;  // South
            case '2': dx = -length; break; // West
            case '3': dy = -length; break; // North
        }

        current.x += dx;
        current.y += dy;
        vertices.push_back(current);
    }
    inputFile.close();

    long long area = 0;
    int n = vertices.size();

    // Shoelace formula - Loop correctly handles the wrap-around
    // as vertices[n-1] connects back to vertices[0] implicitly in the formula.
    // Note: vertices vector has n points if input had n-1 lines.
    // The Python version had n points for n lines, starting at (0,0). C++ does the same.
    // So if there are N lines, there are N+1 vertices.
     n = vertices.size(); // Number of vertices = number of lines + 1
     for (int i = 0; i < n; ++i) {
         int next_idx = (i + 1) % n; // Handles wrap around from last vertex to first
         area += vertices[i].x * vertices[next_idx].y;
         area -= vertices[i].y * vertices[next_idx].x;
     }

    area = abs_ll(area) / 2;

    long long total_points = area + perimeter / 2 + 1;

    std::cout << total_points << std::endl;

    return 0;
}
