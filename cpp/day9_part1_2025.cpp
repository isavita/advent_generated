#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>

struct Point { int x, y; };

int main() {
    std::ifstream fin("input.txt");
    if (!fin) return 1;

    std::string content((std::istreambuf_iterator<char>(fin)), std::istreambuf_iterator<char>());
    for (char &c : content) if (c == ',') c = ' ';
    std::istringstream iss(content);

    std::vector<Point> pts;
    int a, b;
    while (iss >> a >> b) pts.push_back({a, b});

    long long best = 0;
    for (size_t i = 0; i < pts.size(); ++i) {
        int x1 = pts[i].x;
        int y1 = pts[i].y;
        for (size_t j = i; j < pts.size(); ++j) {
            int dx = x1 - pts[j].x;
            if (dx < 0) dx = -dx;
            int dy = y1 - pts[j].y;
            if (dy < 0) dy = -dy;
            long long area = (long long)(dx + 1) * (dy + 1);
            if (area > best) best = area;
        }
    }

    std::cout << best << "\n";
    return 0;
}